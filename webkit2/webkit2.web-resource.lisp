;;; webkit2.web-resource.lisp --- bindings for WebKitWebResource

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-g-object-class* "WebKitWebResource" webkit-web-resource
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "webkit_web_resource_get_type")
  (("response" "WebKitURIResponse")
   ("uri" "gchararray")))
