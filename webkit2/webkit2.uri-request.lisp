;;; webkit2.uri-request.lisp --- bindings for WebKitURIRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitURIRequest"
    ;; XXX: until medial->delim can handle consequtive uppers
    (:type-initializer "webkit_uri_request_get_type")
  (("uri" "gchararray" t t)))
