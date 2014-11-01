;;; webkit2.web-resource.lisp --- bindings for WebKitWebResource

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitWebResource" ()
  (("response" "WebKitURIResponse")
   ("uri" "gchararray")))
