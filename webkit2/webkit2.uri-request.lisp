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

(defclass webkit-uri-request-class (g-object)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "WebKitURIRequest"))

(export 'webkit-uri-request-class)

(defcfun "webkit_uri_request_new" (g-object webkit-uri-request-class)
  (uri :string))
(export 'webkit-uri-request-new)

(defcfun "webkit_uri_request_set_uri" :void
  (request (g-object webkit-uri-request-class))
  (uri :string))
(export 'webkit-uri-request-set-uri)
