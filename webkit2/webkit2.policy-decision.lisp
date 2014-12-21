;;; webkit2.policy-decision.lisp --- bindings for WebKitPolicyDecision

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitPolicyDecision" () ())

(defcfun "webkit_policy_decision_download" :void
  (decision (g-object webkit-policy-decision)))
(export 'webkit-policy-decision-download)

(defcfun "webkit_policy_decision_ignore" :void
  (decision (g-object webkit-policy-decision)))
(export 'webkit-policy-decision-ignore)

(defcfun "webkit_policy_decision_use" :void
  (decision (g-object webkit-policy-decision)))
(export 'webkit-policy-decision-use)
