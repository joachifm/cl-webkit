;;; webkit2.navigation-policy-decision.lisp --- bindings for WebKitNavigationPolicyDecision

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitNavigationPolicyDecision"
  (:superclass webkit-policy-decision)
  (("frame-name" "gchararray")
   ("modifiers" "guint")
   ("mouse-button" "guint")
   ("navigation-type" "WebKitNavigationType")
   ("request" "WebKitURIRequest")))

(define-g-enum "WebKitNavigationType" webkit-navigation-type ()
  :webkit-navigation-type-link-clicked
  :webkit-navigation-type-form-submitted
  :webkit-navigation-type-back-forward
  :webkit-navigation-type-reload
  :webkit-navigation-type-form-resubmitted
  :webkit-navigation-type-other)
