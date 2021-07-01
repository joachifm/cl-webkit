;;; jscore.js-value.lisp --- bindings for JSCValue

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

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
  (context (g-object jsc-context)))
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
  (context (g-object jsc-context)))
(export 'jsc-value-new-boolean)

(defcfun "jsc_value_is_boolean" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-boolean)

(defcfun "jsc_value_to_boolean" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-to-boolean)

(defcfun "jsc_value_new_string" (g-object jsc-value)
  (value (g-object jsc-value)))
(export 'jsc-value-new-string)

;; TODO: jsc_value_new_string_from_bytes

(defcfun "jsc_value_is_string" :boolean
  (value (g-object jsc-value)))
(export 'jsc-value-is-string)

(defcfun "jsc_value_to_string" :string
  (value (g-object jsc-value)))
(export 'jsc-value-to-string)

;; TODO: jsc_value_new_array

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
  (index :int)
  (property (g-object jsc-value)))
(export 'jsc_value_object_set-property-at-index)

(defcfun "jsc_value_object_get_property_at_index" (g-object jsc-value)
  (value (g-object jsc-value))
  (index :int))
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

;; TODO:
;; JSCValue *	jsc_value_object_invoke_method ()
;; JSCValue *	jsc_value_object_invoke_methodv ()
;; void	jsc_value_object_define_property_data ()
;; void	jsc_value_object_define_property_accessor ()
;; JSCValue *	jsc_value_new_function ()
;; JSCValue *	jsc_value_new_functionv ()
;; JSCValue *	jsc_value_new_function_variadic ()
;; JSCValue *	jsc_value_function_call ()
;; JSCValue *	jsc_value_function_callv ()
;; JSCValue *	jsc_value_constructor_call ()
;; JSCValue *	jsc_value_constructor_callv ()

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
  (indent :int))
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

(defvar *js-function-value* :function
  "The value used when translating function values from JavaScript to Lisp.
Likely to get deprecated.")
(export '*js-function-value*)

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
                             (:function-value t)
                             (:array-type (or null (member :list :vector)))
                             (:object-type (or null (member :alist :plist :hash-table)))))
                jsc-value-to-lisp))
(defun jsc-value-to-lisp (jsc-value &key (null-value *js-null-value*)
                                      (undefined-value *js-undefined-value*)
                                      (false-value *js-false-value*)
                                      (true-value *js-true-value*)
                                      (function-value *js-function-value*)
                                      (array-type *js-array-type*)
                                      (object-type *js-object-type*))
  "Translate a JSC-VALUE to a lisp value.
Translates:
- JS strings to strings.
- JS numbers to floating numbers.
- true to TRUE-VALUE (t by default). Also see `*js-true-value*'.
- false to FALSE-VALUE (nil by default). Also see `*js-false-value*'.
- null to NULL-VALUE (:null by default). Also see `*js-null-value*'.
- undefined to UNDEFINED-VALUE (:undefined by default). Also see `*js-undefined-value*'.
- JS functions to FUNCTION-VALUE (:function by default). This will change in the future!
  Also see `*js-function-value*'.
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
                               (cons (cffi:foreign-string-to-lisp property-name) property-value)))
                   into properties
                 finally (return
                           (prog1
                               properties
                             (dotimes (i index)
                               (cffi:foreign-array-free
                                (cffi:mem-aref property-names '(:pointer (:pointer :char)) i)))
                             (cffi:foreign-array-free property-names))))))
    (cond
      ((jsc-value-is-null jsc-value) null-value)
      ((jsc-value-is-undefined jsc-value) undefined-value)
      ((jsc-value-is-function jsc-value) function-value)
      ((jsc-value-is-number jsc-value) (jsc-value-to-double jsc-value))
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
      ((jsc-value-is-object jsc-value)
       (let ((properties (drain-properties jsc-value nil)))
         (case object-type
           (:hash-table
            (loop with object = (make-hash-table :test 'equal)
                  for (name . value) in properties
                  do (setf (gethash name object) value)
                  finally (return object)))
           (:alist properties)
           (:plist (apply #'append (mapcar
                                    (lambda (property)
                                      (cons (intern (first property) :keyword)
                                            (rest property)))
                                    properties)))))))))
