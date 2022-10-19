;;; webkit2.uri-scheme-respose.lisp --- bindings for WebKitURISchemeResponse

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitURISchemeResponse" ()
    (("stream" "GInputStream" t nil)
     ("stream-length" "guint64" t nil)))

(defcfun "webkit_uri_scheme_response_new" (g-object webkit-uri-scheme-response)
  (input-stream (g-object g-memory-input-stream))
  (stream-length :ulong))

(defcfun "webkit_uri_scheme_response_set_content_type" :void
  (response (g-object webkit-uri-scheme-response))
  (content-type :string))

(defcfun "webkit_uri_scheme_response_set_status" :void
  (response (g-object webkit-uri-scheme-response))
  (status-code :uint)
  (reason-phrase :string))

(defcfun "webkit_uri_scheme_response_set_http_headers" :void
  (response (g-object webkit-uri-scheme-response))
  (headers (:pointer (:struct soup-message-headers))))
