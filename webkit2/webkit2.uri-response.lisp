;;; webkit2.uri-response.lisp --- bindings for WebKitURIResponse

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitURIResponse" ()
  (("content-length" "guint")
   ("http-headers" soup-message-headers)
   ("mime-type" "gchararray")
   ("status-code" "guint")
   ("suggested-filename" "gchararray")
   ("uri" "gchararray")))
