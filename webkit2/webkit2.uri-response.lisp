;;; webkit2.uri-response.lisp --- bindings for WebKitURIResponse

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitURIResponse"
    ;; XXX: until medial->delim can handle consequtive uppers
    (:type-initializer "webkit_uri_response_get_type")
  (("content-length" "guint")
   ("mime-type" "gchararray")
   ("status-code" "guint")
   ("suggested-filename" "gchararray")
   ("uri" "gchararray")))
