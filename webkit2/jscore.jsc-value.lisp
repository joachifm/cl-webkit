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
