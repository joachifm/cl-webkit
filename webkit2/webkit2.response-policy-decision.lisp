;;; webkit2.response-policy-decision.lisp --- bindings for WebKitResponsePolicyDecision

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitResponsePolicyDecision"
  (:superclass webkit-policy-decision)
  (("request" "WebKitURIRequest")
   ("response" "WebKitURIResponse")))

(defcfun "webkit_response_policy_decision_is_mime_type_supported" :boolean
  (decision (g-object webkit-response-policy-decision)))
(export 'webkit-response-policy-decision-is-mime-type-supported)
