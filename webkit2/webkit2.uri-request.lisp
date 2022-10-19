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

(defcfun "webkit_uri_request_new" (g-object webkit-uri-request)
  (uri :string))
(export 'webkit-uri-request-new)

(defcfun "webkit_uri_request_get_uri" :string
  (request (g-object webkit-uri-request)))
(export 'webkit-uri-request-get-uri)

(defcfun "webkit_uri_request_set_uri" :void
  (request (g-object webkit-uri-request))
  (uri :string))
(export 'webkit-uri-request-set-uri)

(defcfun "webkit_uri_request_get_http_method" :string
  (request (g-object webkit-uri-request)))
(export 'webkit-uri-request-get-http-method)

(defcfun "webkit_uri_request_get_http_headers" (:pointer (:struct soup-message-headers))
  (request (g-object webkit-uri-request)))
(export 'webkit-uri-request-get-http-headers)
