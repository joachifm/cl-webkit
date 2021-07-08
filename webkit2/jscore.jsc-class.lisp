;;; jscore.jsc-class.lisp --- bindings for JSCClass

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "JSCClass"
    (:type-initializer "jsc_class_get_type")
    (("context" "JSCContext")
     ("name" "gchararray")
     ("parent" "JSCClass")))

(defcfun "jsc_class_get_name" :string
  (class (g-object jsc-class)))
(export 'jsc-class-get-name)
(defcfun "jsc_class_get_parent" (g-object jsc-class)
  (class (g-object jsc-class)))
(export 'jsc-class-get-parent)

(defcfun "jsc_class_add_constructorv" (g-object jsc-value)
  (class (g-object jsc-class))
  (name :string)
  (callback :pointer) ;; XXX: GCallback
  (user-data :pointer)
  (destroy-notify :pointer) ;; XXX: GDestroyNotify
  (return-type :pointer) ;; XXX: GType
  (n-parameters :uint)
  (parameter-types (:pointer :pointer)))
(export 'jsc-class-add-constructorv)

(defcfun "jsc_class_add_constructor_variadic" (g-object jsc-value)
  (class (g-object jsc-class))
  (name :string)
  (callback :pointer) ;; XXX: GCallback
  (user-data :pointer)
  (destroy-notify :pointer) ;; XXX: GDestroyNotify
  (return-type :pointer)) ;; XXX: GType
(export 'jsc-class-add-constructor-variadic)

(defcfun "jsc_class_add_methodv" (g-object jsc-value)
  (class (g-object jsc-class))
  (name :string)
  (callback :pointer) ;; XXX: GCallback
  (user-data :pointer)
  (destroy-notify :pointer) ;; XXX: GDestroyNotify
  (return-type :pointer) ;; XXX: GType
  (n-parameters :uint)
  (parameter-types (:pointer :pointer)))
(export 'jsc-class-add-methodv)

(defcfun "jsc_class_add_method_variadic" (g-object jsc-value)
  (class (g-object jsc-class))
  (name :string)
  (callback :pointer) ;; XXX: GCallback
  (user-data :pointer)
  (destroy-notify :pointer) ;; XXX: GDestroyNotify
  (return-type :pointer)) ;; XXX: GType
(export 'jsc-class-add-method-variadic)

(defcfun "jsc_class_add_property" :void
  (class (g-object jsc-class))
  (name :string)
  (property-type :pointer) ;; XXX: GType
  (getter :pointer) ;; XXX: GCallback
  (setter :pointer) ;; XXX: GCallback
  (user-data :pointer)
  (destroy-notify :pointer)) ;; XXX: GDestroyNotify
(export 'jsc-class-add-property)
