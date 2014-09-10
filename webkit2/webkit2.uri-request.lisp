;;; webkit2.uri-request.lisp --- bindings for WebKitURIRequest

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitURIRequest" ()
  (("uri" "gchararray" t t)))
