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

(defclass webkit-web-resource (g-object)
  ((response
    :allocation :gobject-property
    :g-property-name "response"
    :g-property-type webkit-uri-response
    :reader web-resource-response)
   (uri
    :allocation :gobject-property
    :g-property-name "uri"
    :g-property-type :string
    :reader web-resource-uri))
  (:metaclass gobject-class)
  (:g-type-name . "WebKitWebResource")
  (:g-type-initializer . "webkit_web_resource_get_type"))

(export 'webkit-web-resource)
(export 'web-resource-response)
(export 'web-resource-uri)
