;;; webkit2.form-submission-request.lisp --- bindings for WebKitFormSubmissionRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitFormSubmissionRequest" () ())

(defcfun "webkit_form_submission_request_get_text_fields" :pointer ; XXX: GHashTable
  (request (g-object webkit-form-submission-request)))
(export 'webkit-form-submission-request-get-text-fields)

(defcfun "webkit_form_submission_request_submit" :void
  (request (g-object webkit-form-submission-request)))
(export 'webkit-form-submission-request-submit)
