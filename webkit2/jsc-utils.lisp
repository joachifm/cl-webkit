;;; jsc-utils.lisp --- Utilities on top of JSCore.

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(defmethod print-object ((val jsc-value) stream)
  (print-unreadable-object (val stream :type t)
    (format stream "~a: ~a"
            (cond
              ((jsc-value-is-null val) "null")
              ((jsc-value-is-undefined val) "undefined")
              ((jsc-value-is-boolean val) "bool")
              ((jsc-value-is-number val) "number")
              ((jsc-value-is-string val) "string")
              ((jsc-value-is-array val) "array")
              ((jsc-value-is-function val) "function")
              ((jsc-value-is-object val) "object"))
            (jsc-value-to-lisp val))))

(defmethod print-object ((val jsc-class) stream)
  (print-unreadable-object (val stream :type t)
    (format stream "~a" (jsc-class-get-name val))))

(defvar %context-lock (bt:make-lock))
(defvar %context nil)

(export 'get-jsc-context)
(declaim (ftype (function ((or null webkit-web-view) &optional (or null string jsc-context)))
                get-jsc-context))
(defun get-jsc-context (view &optional designator)
  "Get a JSCContext for VIEW, matching DESIGNATOR.
DESIGNATOR could be:
- A `jsc-context' -- return it back.
- A string -- return a context of the world named by DESIGNATOR.
- A nil -- return the default context of VIEW."
  (bt:with-lock-held (%context-lock)
    (let ((context-callback
            (lambda (result jsc-value)
              (declare (ignore result))
              (setf %context (jsc-value-get-context jsc-value)))))
      (gtk:within-gtk-thread
        (typecase designator
          (jsc-context (setf %context designator))
          (string (webkit:webkit-web-view-evaluate-javascript
                   view "null" context-callback nil designator))
          (null (webkit:webkit-web-view-evaluate-javascript
                 view "null" context-callback))))
      (loop until %context
            finally (return (prog1 %context
                              (setf %context nil)))))))

(defvar *js-null-value* :null
  "The value used when translating null from JavaScript to Lisp.")
(export '*js-null-value*)

(defvar *js-undefined-value* :undefined
  "The value used when translating undefined from JavaScript to Lisp.")
(export '*js-undefined-value*)

(defvar *js-false-value* nil
  "The value used when translating false from JavaScript to Lisp.")
(export '*js-false-value*)

(defvar *js-true-value* t
  "The value used when translating true from JavaScript to Lisp.")
(export '*js-true-value*)

(defvar *js-array-type* :list
  "The Lisp data type used when translating arrays from JavaScript to Lisp.")
(export '*js-array-type*)

(defvar *js-object-type* :hash-table
  "The Lisp data type used when translating objects from JavaScript to Lisp.")
(export '*js-object-type*)

(export 'jsc-value-to-lisp)
(declaim (ftype (function (t &key (:null-value t)
                             (:undefined-value t)
                             (:false-value t)
                             (:true-value t)
                             (:array-type (or null (member :list :vector)))
                             (:object-type (or null (member :alist :plist :hash-table)))))
                jsc-value-to-lisp))
(defun jsc-value-to-lisp (jsc-value &key (null-value *js-null-value*)
                                      (undefined-value *js-undefined-value*)
                                      (false-value *js-false-value*)
                                      (true-value *js-true-value*)
                                      (array-type *js-array-type*)
                                      (object-type *js-object-type*))
  "Translate a JSC-VALUE to a lisp value.
Translates:
- JS strings to strings.
- JS numbers:
  - to integers if the absolute value is less than 1.0d22,
  - floating-point numbers otherwise.
- true to TRUE-VALUE (t by default). Also see `*js-true-value*'.
- false to FALSE-VALUE (nil by default). Also see `*js-false-value*'.
- null to NULL-VALUE (:null by default). Also see `*js-null-value*'.
- undefined to UNDEFINED-VALUE (:undefined by default). Also see `*js-undefined-value*'.
- JS arrays to either lists or vectors (:list and :vector ARRAY-TYPE respectively).
  Also see `*js-array-type*'.
- JS objects (also see `*js-object-type*') to:
  - hash-tables (if OBJECT-TYPE is :hash-table, default) with string keys,
  - alists (if OBJECT-TYPE is :alist) with string keys,
  - plists (if OBJECT-TYPE is :plist) with keyword keys,"
  (flet ((drain-properties (jsc-value array-p)
           (loop with property-names = (jsc-value-object-enumerate-properties jsc-value)
                 for index from 0
                 until (cffi:null-pointer-p property-names)
                 for property-name = (cffi:mem-aref property-names '(:pointer (:pointer :char)) index)
                 until (cffi:null-pointer-p property-name)
                 collect (let ((property-value
                                 (jsc-value-to-lisp
                                  (if array-p
                                      (jsc-value-object-get-property-at-index jsc-value index)
                                      (jsc-value-object-get-property jsc-value property-name)))))
                           (if array-p
                               property-value
                               (list (cffi:foreign-string-to-lisp property-name) property-value)))
                   into properties
                 finally (return
                           (progn
                             (dotimes (i index)
                               (cffi:foreign-array-free
                                (cffi:mem-aref property-names '(:pointer (:pointer :char)) i)))
                             (cffi:foreign-array-free property-names)
                             properties)))))
    (cond
      ((jsc-value-is-null jsc-value) null-value)
      ((jsc-value-is-undefined jsc-value) undefined-value)
      ((jsc-value-is-number jsc-value) (let ((num (jsc-value-to-double jsc-value)))
                                         (handler-case
                                             ;; Floats get too chaotic after 1.0d22.
                                             ;; Looking at floating part doesn't help there.
                                             (if (and (< (abs num) 1.0d22) (zerop (rem num 1.0)))
                                                 (truncate num)
                                                 num)
                                           ;; Truncation/comparison can error out on infinity/NaN.
                                           (error (c) (declare (ignore c)) num))))
      ((jsc-value-is-string jsc-value) (jsc-value-to-string jsc-value))
      ((jsc-value-is-boolean jsc-value)
       (if (jsc-value-to-boolean jsc-value)
           true-value
           false-value))
      ((jsc-value-is-array jsc-value)
       (let ((elements (drain-properties jsc-value t)))
         (if (eq array-type :list)
             elements
             (coerce elements 'vector))))
      ((jsc-value-is-function jsc-value)
       (lambda (&rest args)
         (jsc-value-to-lisp
          (jsc-value-function-callv
           jsc-value
           (length args)
           (if args
               ;; TODO: Free this.
               (cffi:foreign-alloc :pointer
                                   :initial-contents
                                   (mapcar #'pointer
                                           (mapcar (lambda (arg)
                                                     (lisp-to-jsc-value
                                                      arg (jsc-value-get-context jsc-value)))
                                                   args))
                                   :count (length args))
               (null-pointer))))))
      ((jsc-value-is-object jsc-value)
       (let ((properties (drain-properties jsc-value nil)))
         (case object-type
           (:hash-table
            (loop with object = (make-hash-table :test 'equal)
                  for (name value) in properties
                  do (setf (gethash name object) value)
                  finally (return object)))
           (:alist properties)
           (:plist (apply #'append (mapcar
                                    (lambda (property)
                                      (list (intern (string-upcase (first property)) :keyword)
                                            (second property)))
                                    properties)))))))))

(export 'lisp-to-jsc-value)
(defgeneric lisp-to-jsc-value (value &optional context)
  (:method ((value t) &optional (context (jsc-context-get-current)))
    (declare (ignore value))
    (jsc-value-new-undefined context))
  (:documentation "Transform a provided Lisp value to the most sensible JSCValue counterpart.
In case no suitable method was found, create a JSCValue for undefined."))

(defmethod lisp-to-jsc-value ((number real) &optional (context (jsc-context-get-current)))
  (jsc-value-new-number context (coerce number 'double-float)))

(defmethod lisp-to-jsc-value ((string string) &optional (context (jsc-context-get-current)))
  (jsc-value-new-string context string))

(defmethod lisp-to-jsc-value ((symbol symbol) &optional (context (jsc-context-get-current)))
  (cond
    ((eq symbol *js-null-value*) (jsc-value-new-null context))
    ((eq symbol *js-undefined-value*) (jsc-value-new-undefined context))
    ((eq symbol *js-true-value*) (jsc-value-new-boolean context t))
    ((eq symbol *js-false-value*) (jsc-value-new-boolean context nil))
    (t (call-next-method (symbol-name symbol) context))))

(defmethod lisp-to-jsc-value ((list list) &optional (context (jsc-context-get-current)))
  (jsc-value-new-array-from-list
   context (mapcar (lambda (value) (lisp-to-jsc-value value context)) list)))

(defmethod lisp-to-jsc-value ((vector vector) &optional (context (jsc-context-get-current)))
  (jsc-value-new-array-from-list
   context (map 'list (lambda (value) (lisp-to-jsc-value value context)) vector)))

(defmethod lisp-to-jsc-value ((hash-table hash-table) &optional (context (jsc-context-get-current)))
  (let (json-alist)
    (maphash
     (lambda (key value)
       (push (list key (jsc-value-to-json (lisp-to-jsc-value value context) 0)) json-alist))
     hash-table)
    (jsc-value-new-from-json context (format nil "{~{~s:~a~}~:{,~s:~a~}}" (first json-alist) (rest json-alist)))))

(declaim (ftype (function (webkit-web-view (or null string jsc-context) t t integer)) %make-jsc-function))
(defun %make-jsc-function (view context-designator name callback n-args)
  (let ((jsc-value-type (foreign-funcall "jsc_value_get_type" :pointer)))
    (jsc-value-to-lisp
     (jsc-value-new-functionv
      (get-jsc-context view context-designator) name callback
      (cffi:null-pointer) (cffi:null-pointer) ;; TODO: Use g-notify-destroy-free?
      jsc-value-type n-args
      (if (not (zerop n-args))
          (cffi:foreign-alloc
           :pointer :initial-contents (loop repeat n-args collect jsc-value-type)
                    :count n-args)
          (make-pointer (g-type-make-fundamental 1)))))))

(export 'make-jsc-function)
(defmacro make-jsc-function ((view &optional name context-designator) args &body body)
  "Create a new JSCValue function with a callback having ARGS and BODY.

The function is only defined in the VIEW, in the JSCContext designated
by CONTEXT-DESIGNATOR, with the NAME assigned to it.

NAME is either a symbol, a string, or nil:
- Symbol turn into a camelcase JS function name.
- String names new function directly.
- nil gives no name to the new function, making it anonymous.

See `get-jsc-context' for what CONTEXT-DESIGNATOR could be."
  (let* ((js-name (etypecase name
                    (string name)
                    (null (null-pointer))
                    (symbol (cffi:translate-camelcase-name name))))
         (callback-name (gensym "js-callback"))
         (n-args (length args))
         (user-data (gensym)))
    `(progn
       (defcallback ,callback-name (g-object jsc-value)
           (,@(loop for arg in args collect `(,arg :pointer)) (,user-data :pointer))
         (declare (ignorable ,user-data ,@args))
         (let (,@(loop for arg in args collect `(,arg (jsc-value-to-lisp ,arg))))
           (lisp-to-jsc-value
            (progn
              ,@body))))
       (%make-jsc-function ,view ,context-designator ,js-name (cffi:callback ,callback-name) ,n-args))))
