;;; jscore.js-value.lisp --- bindings for JSCValue

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-g-enum "JSCValuePropertyFlags" jsc-value-property-flags ()
  (:jsc-value-property-configurable 1)
  (:jsc-value-property-enumberable 2)
  (:jsc-value-property-writable 4))

(define-webkit-class "JSCValue"
    (:type-initializer "jsc_value_get_type")
    (("context" "JSCContext")))

(defcfun "jsc_value_get_context" (g-object jsc-context)
  (value (g-object jsc-value)))
(export 'jsc-value-get-context)

(defcfun "jsc_value_new_undefined" (g-object jsc-value)
  (context (g-object jsc-context)))
(export 'jsc-value-new-undefined)

(defcfun "jsc_value_is_undefined" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-undefined)

(defcfun "jsc_value_new_null" (g-object jsc-value)
  (context (g-object jsc-context)))
(export 'jsc-value-new-null)

(defcfun "jsc_value_is_null" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-null)

(defcfun "jsc_value_new_number" (g-object jsc-value)
  (context (g-object jsc-context))
  (number :double))
(export 'jsc-value-new-number)

(defcfun "jsc_value_is_number" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-number)

(defcfun "jsc_value_to_double" :double
  (value (g-object jsc-value)))
(export 'jsc-value-to-double)

(defcfun "jsc_value_to_int32" :int
  (value (g-object jsc-value)))
(export 'jsc-value-to-int32)

(defcfun "jsc_value_new_boolean" (g-object jsc-value)
  (context (g-object jsc-context))
  (value :boolean))
(export 'jsc-value-new-boolean)

(defcfun "jsc_value_is_boolean" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-boolean)

(defcfun "jsc_value_to_boolean" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-to-boolean)

(defcfun "jsc_value_new_string" (g-object jsc-value)
  (context (g-object jsc-context))
  (value :string))
(export 'jsc-value-new-string)

;; TODO: jsc_value_new_string_from_bytes

(defcfun "jsc_value_is_string" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-string)

(defcfun "jsc_value_to_string" :string
  (value (g-object jsc-value)))
(export 'jsc-value-to-string)

;; TODO: jsc_value_new_array

(defcstruct g-ptr-array
  (data :pointer)
  (length :uint))

(defcfun "jsc_value_new_array_from_garray" (g-object jsc-value)
  (context (g-object jsc-context))
  (array :pointer))

(defun jsc-value-new-array-from-list (context list)
  "Create a JSCValue array in CONTEXT out of LIST of JSCValues."
  (let* ((list-values (if (null list)
                          (null-pointer)
                          (foreign-alloc :pointer :initial-contents (mapcar #'pointer list)
                                                  :count (length list)))))
    (with-foreign-object (array '(:struct g-ptr-array))
      (setf (foreign-slot-value array '(:struct g-ptr-array) 'data) list-values
            (foreign-slot-value array '(:struct g-ptr-array) 'length) (length list))
      (jsc-value-new-array-from-garray context array))))

(defcfun "jsc_value_is_array" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-array)

(defcfun "jsc_value_new_object" (g-object jsc-value)
  (context (g-object jsc-context))
  (instance :pointer)
  (class (g-object jsc-class)))
(export 'jsc-value-new-object)

(defcfun "jsc_value_is_object" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-object)

(defcfun "jsc_value_object_is_instance_of" :boolean
  (value (g-object jsc-value))
  (name :string))
(export 'jsc_value_object-is-instance-of)

(defcfun "jsc_value_object_set_property" :void
  (value (g-object jsc-value))
  (name :string)
  (property (g-object jsc-value)))
(export 'jsc_value-object-set-property)

(defcfun "jsc_value_object_get_property" (g-object jsc-value)
  (value (g-object jsc-value))
  (name :string))
(export 'jsc_value-object-get-property)

(defcfun "jsc_value_object_set_property_at_index" :void
  (value (g-object jsc-value))
  (index :uint)
  (property (g-object jsc-value)))
(export 'jsc_value_object_set-property-at-index)

(defcfun "jsc_value_object_get_property_at_index" (g-object jsc-value)
  (value (g-object jsc-value))
  (index :uint))
(export 'jsc_value_object_get-property-at-index)

(defcfun "jsc_value_object_has_property" :boolean
  (value (g-object jsc-value))
  (name :string))
(export 'jsc_value-object-has-property)

(defcfun "jsc_value_object_delete_property" :boolean
  (value (g-object jsc-value))
  (name :string))
(export 'jsc_value-object-delete-property)

(defcfun "jsc_value_object_enumerate_properties" (:pointer :string)
  (value (g-object jsc-value)))
(export 'jsc_value-object-enumerate-properties)

(defcfun "jsc_value_object_invoke_methodv" (g-object jsc-value)
  (value (g-object jsc-value))
  (name :string)
  (n-parameters :uint)
  (parameters (:pointer (g-object jsc-value))))
(export 'jsc-value-object-invoke-methodv)

(defcfun "jsc_value_object_define_property_data" :void
  (value (g-object jsc-value))
  (property-name :string)
  (property-flags :uint)
  (property-value (g-object jsc-value)))
(export 'jsc-value-object-define-property-data)

(defcfun "jsc_value_object_define_property_accessor" :void
  (value (g-object jsc-value))
  (property-name :string)
  (property-flags :uint)
  (property-type :pointer)
  (getter :pointer) ;; XXX: GCallback
  (setter :pointer) ;; XXX: GCallback
  (user-data :pointer)
  (destroy-notify :pointer)) ;; XXX: GDestroyNotify
(export 'jsc-value-object-define-property-accessor)

(defcfun "jsc_value_new_functionv" (g-object jsc-value)
  (context (g-object jsc-context))
  (name :string)
  (callback :pointer) ;; XXX: GCallback
  (user-data :pointer)
  (destroy-notify :pointer) ;; XXX: GDestroyNotify
  (return-type :pointer)
  (n-parameters :uint)
  (parameter-types :pointer))
(export 'jsc-value-new-functionv)

(defcfun "jsc_value_new_function_variadic" (g-object jsc-value)
  (context (g-object jsc-context))
  (name :string)
  (callback :pointer) ;; XXX: GCallback
  (user-data :pointer)
  (destroy-notify :pointer) ;; XXX: GDestroyNotify
  (return-type :pointer))
(export 'jsc-value-new-function-variadic)

(defmacro define-jsc-function (name-and-options (&rest args) &body body)
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
               (declare (ignore user-data))
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
                       (cffi:null-pointer) (cffi:null-pointer)
                       ,jsc-value-type ,n-args
                       (cffi:foreign-alloc
                        :pointer :initial-contents (list ,@(loop for i below n-args collect jsc-value-type)))))))
               (defun ,lispy-name (,@args)
                 ,documentation
                 (funcall ,lispy-name ,@args)))))))))
(export 'define-jsc-function)

(defcfun "jsc_value_function_callv" (g-object jsc-value)
  (value (g-object jsc-value))
  (n-parameters :uint)
  (parameters (:pointer (g-object jsc-value))))
(export 'jsc-value-function-callv)

(defcfun "jsc_value_constructor_callv" (g-object jsc-value)
  (value (g-object jsc-value))
  (n-parameters :uint)
  (parameters (:pointer (g-object jsc-value))))
(export 'jsc-value-constructor-callv)

(defcfun "jsc_value_is_function" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-function)

(defcfun "jsc_value_is_constructor" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-constructor)

(defcfun "jsc_value_new_from_json" (g-object jsc-value)
  (context (g-object jsc-context))
  (json :string))
(export 'jsc_value-new-from-json)

(defcfun "jsc_value_to_json" :string
  (value (g-object jsc-value))
  (indent :uint))
(export 'jsc-value-to-json)

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

(defmethod lisp-to-jsc-value ((value t) &optional (context (jsc-context-get-current)))
  (declare (ignore value))
  (jsc-value-new-undefined context))

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
