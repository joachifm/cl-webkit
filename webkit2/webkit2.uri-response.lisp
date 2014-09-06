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

(defclass webkit-uri-response (g-object)
  (
   (content-length
    :allocation :gobject-property
    :g-property-name "content-length"
    :g-property-type :int
    :reader uri-response-content-length)
   (mime-type
    :allocation :gobject-property
    :g-property-name "mime-type"
    :g-property-type :string
    :reader uri-response-mime-type)
   (status-code
    :allocation :gobject-property
    :g-property-name "status-code"
    :g-property-type :int
    :reader uri-response-status-code)
   (suggested-filename
    :allocation :gobject-property
    :g-property-name "suggested-filename"
    :g-property-type :string
    :reader uri-response-suggested-filename)
   (uri
    :allocation :gobject-property
    :g-property-name "uri"
    :g-property-type :string
    :reader uri-response-uri)
   )
  (:metaclass gobject-class)
  (:g-type-name . "WebKitURIResponse")
  (:g-type-initializer . "webkit_uri_response_get_type"))

(export 'webkit-uri-response)
(export 'uri-response-content-length)
(export 'uri-response-mime-type)
(export 'uri-response-status-code)
(export 'uri-response-suggested-filename)
(export 'uri-response-uri)
