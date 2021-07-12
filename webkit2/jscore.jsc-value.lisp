;;; jscore.jsc-value.lisp --- bindings for JSCValue

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
(export 'jsc-value-object-is-instance-of)

(defcfun "jsc_value_object_set_property" :void
  (value (g-object jsc-value))
  (name :string)
  (property (g-object jsc-value)))
(export 'jsc-value-object-set-property)

(defcfun "jsc_value_object_get_property" (g-object jsc-value)
  (value (g-object jsc-value))
  (name :string))
(export 'jsc-value-object-get-property)

(defcfun "jsc_value_object_set_property_at_index" :void
  (value (g-object jsc-value))
  (index :uint)
  (property (g-object jsc-value)))
(export 'jsc-value_object_set-property-at-index)

(defcfun "jsc_value_object_get_property_at_index" (g-object jsc-value)
  (value (g-object jsc-value))
  (index :uint))
(export 'jsc-value_object_get-property-at-index)

(defcfun "jsc_value_object_has_property" :boolean
  (value (g-object jsc-value))
  (name :string))
(export 'jsc-value-object-has-property)

(defcfun "jsc_value_object_delete_property" :boolean
  (value (g-object jsc-value))
  (name :string))
(export 'jsc-value-object-delete-property)

(defcfun "jsc_value_object_enumerate_properties" (:pointer :string)
  (value (g-object jsc-value)))
(export 'jsc-value-object-enumerate-properties)

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
(export 'jsc-value-new-from-json)

(defcfun "jsc_value_to_json" :string
  (value (g-object jsc-value))
  (indent :uint))
(export 'jsc-value-to-json)
