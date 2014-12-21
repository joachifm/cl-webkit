;;; webkit2.uri-request.lisp --- bindings for WebKitURIRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitURIRequest" ()
  (("uri" "gchararray" t t)))

(defcfun "webkit_uri_request_get_http_headers" soup:soup-message-headers
  (request (g-object webkit-uri-request)))
(export 'webkit-request-get-http-headers)
