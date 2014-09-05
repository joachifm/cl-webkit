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

(defclass webkit-uri-request (g-object)
  ((uri
    :allocation :gobject-property
    :g-property-name "uri"
    :g-property-type :string
    :accessor uri-request-uri
    :initarg :uri))
  (:metaclass gobject-class)
  (:g-type-name . "WebKitURIRequest")
  (:g-type-initializer . "webkit_uri_request_get_type"))

(export 'webkit-uri-request)
(export 'uri-request-uri)
