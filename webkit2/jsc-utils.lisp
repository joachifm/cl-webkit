;;; jsc-utils.lisp --- Utilities on top of JSCore.

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

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

(export 'define-jsc-function)
(defmacro define-jsc-function (name-and-options (&rest args) &body body)
  "Define a new JSCValue function with a callback having ARGS and BODY.
NAME-AND-OPTIONS is either:
- A single symbol treated as a name of the function.
- A list of a form (NAMES &KEY CONTEXT) where NAMES is:
  - A symbol, like above, or
  - A list of (LISPY-NAME &OPTIONAL JS-NAME) where
    the second will be assigned to the created JavaScript function.

In case no JavaScript name is provided, it's generated as camelcase versin of LISPY-NAME."
  (destructuring-bind
      (names &key (context (jsc-context-get-current)))
      (uiop:ensure-list name-and-options)
    (destructuring-bind
        (lispy-name &optional (js-name (cffi:translate-camelcase-name lispy-name)))
        (uiop:ensure-list names)
      (let* ((callback-name (intern (format nil "~a-CALLBACK" lispy-name)))
             (jsc-value-type (foreign-funcall "jsc_value_get_type" :pointer))
             (n-args (length args)))
        (multiple-value-bind (body-forms declarations documentation)
            (uiop:parse-body body :documentation t)
          `(progn
             (defcallback ,callback-name (g-object jsc-value)
                 (,@(loop for arg in args
                          collect `(,arg :pointer)) (user-data :pointer))
               (declare (ignorable user-data ,@args))
               (let (,@(loop for arg in args
                             collect `(,arg (jsc-value-to-lisp ,arg))))
                 (lisp-to-jsc-value
                  (progn
                    ,@declarations
                    ,@body-forms))))
             (let ((,lispy-name
                     (jsc-value-to-lisp
                      (jsc-value-new-functionv
                       ,context ,js-name (cffi:callback ,callback-name)
                       (cffi:null-pointer) (cffi:null-pointer) ;; TODO: Use g-notify-destroy-free?
                       ,jsc-value-type ,n-args
                       (cffi:foreign-alloc
                        :pointer :initial-contents (list ,@(loop for i below n-args collect jsc-value-type)))))))
               (defun ,lispy-name (,@args)
                 ,documentation
                 (funcall ,lispy-name ,@args)))))))))
