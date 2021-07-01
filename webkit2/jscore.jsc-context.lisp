;;; jscore.jsc-context.lisp --- bindings for JSCContext

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "JSCContext"
    (:type-initializer "jsc_context_get_type")
    (("virtual-machine" "JSCVirtualMachine")))

(define-webkit-class "JSCVirtualMachine"
    (:type-initializer "jsc_virtual_machine_get_type")
    ())

(defcfun "jsc_context_new" (g-object jsc-context))
(export 'jsc-context-new)

(defcfun "jsc_context_new_with_virtual_machine" (g-object jsc-context)
  (vm (g-object jsc-virtual-machine)))
(export 'jsc_context_new-with-virtual-machine)

(defcfun "jsc_context_get_virtual_machine" (g-object jsc-virtual-machine))
(export 'jsc_context-get-virtual-machine)

(defcfun "jsc_context_get_exception" (g-object jsc-exception)
  (context (g-object jsc-context)))

;; TODO:
;; void	jsc_context_throw ()
;; void	jsc_context_throw_printf ()
;; void	jsc_context_throw_with_name ()
;; void	jsc_context_throw_with_name_printf ()
;; void	jsc_context_throw_exception ()
;; void	jsc_context_clear_exception ()
;; void	jsc_context_push_exception_handler ()
;; void	jsc_context_pop_exception_handler ()

(defcfun "jsc_context_evaluate" (g-object jsc-value)
  (context (g-object jsc-context))
  (code :string)
  (length :int))
(export 'jsc-context-evaluate)

(defcfun "jsc_context_evaluate_with_source_uri" (g-object jsc-value)
  (context (g-object jsc-context))
  (code :string)
  (length :int)
  (uri :string)
  (line-number :int))
(export 'jsc_context_evaluate-with-source-uri)

(defcfun "jsc_context_evaluate_in_object" (g-object jsc-value)
  (context (g-object jsc-context))
  (code :string)
  (length :int)
  (object-instance :pointer)
  (object-class (g-object jsc-class))
  (uri :string)
  (line-number :int)
  (object (:pointer (g-object jsc-value))))
(export 'jsc_context-evaluate-in-object)

;; TODO: JSCCheckSyntaxResult	jsc_context_check_syntax ()

(defcfun "jsc_context_get_global_object" (g-object jsc-value)
  (context (g-object jsc-context)))
(export 'jsc_context-get-global-object)

(defcfun "jsc_context_set_value" :void
  (context (g-object jsc-context))
  (name :string)
  (value (g-object jsc-value)))
(export 'jsc-context-set-value)

(defcfun "jsc_context_get_value" (g-object jsc-value)
  (context (g-object jsc-context))
  (name :string))
(export 'jsc-context-get-value)

(defcfun "jsc_context_register_class" (g-object jsc-class)
  (context (g-object jsc-context))
  (name :string)
  (parent-class (g-object jsc-class))
  (vtable :pointer) ;; XXX: JSCClassVTable (usually what you want is NULL)
  (destroy-notify :pointer))
(export 'jsc-context-register-class)
