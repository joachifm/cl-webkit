;;; jscore.jsc-exception.lisp --- bindings for JSCException

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "JSCException"
    (:type-initializer "jsc_exception_get_type")
    ())

(defcfun "jsc_exception_new" (g-object jsc-exception)
  (context (g-object jsc-context))
  (message :string))
(export 'jsc-exception-new)

;; TODO (unnecessary?):
;; JSCException *	jsc_exception_new_printf ()
;; JSCException *	jsc_exception_new_vprintf ()

(defcfun "jsc_exception_new_with_name" (g-object jsc-exception)
  (context (g-object jsc-context))
  (name :string)
  (message :string))
(export 'jsc-exception-new-with-name)

;; TODO (unnecessary?):
;; JSCException *	jsc_exception_new_with_name_printf ()
;; JSCException *	jsc_exception_new_with_name_vprintf ()

(defcfun "jsc_exception_get_name" :string
  (exception (g-object jsc-exception)))
(export 'jsc-exception-get-name)

(defcfun "jsc_exception_get_message" :string
  (exception (g-object jsc-exception)))
(export 'jsc-exception-get-message)

(defcfun "jsc_exception_get_line_number" :int
  (exception (g-object jsc-exception)))
(export 'jsc-exception-get-line-number)

(defcfun "jsc_exception_get_column_number" :int
  (exception (g-object jsc-exception)))
(export 'jsc-exception-get-column-number)

(defcfun "jsc_exception_get_source_uri" :string
  (exception (g-object jsc-exception)))
(export 'jsc-exception-source-uri)

(defcfun "jsc_exception_get_backtrace_string" :string
  (exception (g-object jsc-exception)))
(export 'jsc-exception-get-backtrace-string)

(defcfun "jsc_exception_to_string" :string
  (exception (g-object jsc-exception)))
(export 'jsc-exception-to-string)

(defcfun "jsc_exception_report" :string
  (exception (g-object jsc-exception)))
(export 'jsc-exception-report)

(define-condition jsc-exception-condition (error)
  ((exception :initform (error "There should be a JSCException.")
              :accessor exception
              :initarg :exception))
  (:documentation "Condition to signal when a JSCException is raised.")
  (:report (lambda (condition stream)
             (format stream "~a~&~%~%~a"
                     (jsc-exception-report (exception condition))
                     (jsc-exception-get-backtrace-string (exception condition))))))
