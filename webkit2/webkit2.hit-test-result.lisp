;;; webkit2.hit-test-result.lisp --- bindings for WebKitHitTestResult

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitHitTestResult" ()
  (("context" "guint" t t)
   ("image-uri" "gchararray" t t)
   ("link-label" "gchararray" t t)
   ("link-title" "gchararray" t t)
   ("link-uri" "gchararray" t t)
   ("media-uri" "gchararray" t t)))

(define-g-enum "WebKitHitTestResultContext" webkit-hit-test-result-context ()
  :webkit-hit-test-result-context-document
  :webkit-hit-test-result-context-link
  :webkit-hit-test-result-context-image
  :webkit-hit-test-result-context-media
  :webkit-hit-test-result-context-editable
  :webkit-hit-test-result-context-scrollbar
  :webkit-hit-test-result-context-selection)
