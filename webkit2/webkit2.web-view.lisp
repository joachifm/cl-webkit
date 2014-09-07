;;; webkit2.web-view.lisp --- bindings for WebKitWebView

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(defclass webkit-web-view (gtk-widget)
  ((estimated-load-progress
    :allocation :gobject-property
    :g-property-name "estimated-load-progress"
    :g-property-type :double
    :reader web-view-estimated-load-progress)
  (is-loading-p
   :allocation :gobject-property
   :g-property-name "is-loading"
   :g-property-type :bool
   :reader web-view-is-loading-p)
  (title
   :allocation :gobject-property
   :g-property-name "title"
   :g-property-type :string
   :reader web-view-title)
  (uri
   :allocation :gobject-property
   :g-property-name "uri"
   :g-property-type :string
   :reader web-view-uri)
  (view-mode
   :allocation :gobject-property
   :g-property-name "view-mode"
   :g-property-type webkit-view-mode
   :accessor web-view-view-mode)
  (web-context
   :allocation :gobject-property
   :g-property-name "web-context"
   :g-property-type webkit-web-context
   :accessor web-view-web-context
   :initarg :web-context)
  (zoom-level
   :allocation :gobject-property
   :g-property-name "zoom-level"
   :g-property-type :double
   :accessor web-view-zoom-level))
  (:metaclass gobject-class)
  (:g-type-name . "WebKitWebView")
  (:g-type-initializer . "webkit_web_view_get_type"))

(export 'webkit-web-view)

(export 'web-view-estimated-load-progress)
(export 'web-view-is-loading-p)
(export 'web-view-title)
(export 'web-view-uri)
(export 'web-view-view-mode)
(export 'web-view-web-context)
(export 'web-view-zoom-level)

(define-g-enum "WebKitLoadEvent" webkit-load-event ()
  :webkit-load-started
  :webkit-load-redirected
  :webkit-load-committed
  :webkit-load-finished)

(define-g-enum "WebKitViewMode" webkit-view-mode ()
  :webkit-view-mode-web
  :webkit-view-mode-source)

(defcfun "webkit_web_view_load_uri" :void
  (web-view (g-object webkit-web-view))
  (uri :string))
(export 'webkit-web-view-load-uri)

(defcfun "webkit_web_view_load_html" :void
  (web-view (g-object webkit-web-view))
  (content :string)
  (base-uri :string))
(export 'webkit-web-view-load-html)

(defcfun "webkit_web_view_load_alternate_html" :void
  (web-view (g-object webkit-web-view))
  (content :string)
  (content-uri :string)
  (base-uri :string)) ; XXX: can be NULL
(export 'webkit-web-view-load-alternate-html)

(defcfun "webkit_web_view_load_plain_text" :void
  (web-view (g-object webkit-web-view))
  (plain-text :string))
(export 'webkit-web-view-load-plain-text)

(defcfun "webkit_web_view_load_request" :void
  (web-view (g-object webkit-web-view))
  (request (g-object webkit-uri-request)))
(export 'webkit-web-view-load-request)

(defcfun "webkit_web_view_go_back" :void
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-go-back)

(defcfun "webkit_web_view_go_forward" :void
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-go-forward)

(defcfun "webkit_web_view_reload" :void
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-reload)

(defcfun "webkit_web_view_set_settings" :void
  (web-view (g-object webkit-web-view))
  (settings (g-object webkit-settings)))
(export 'webkit-web-view-set-settings)

(defcfun "webkit_web_view_get_settings" (g-object webkit-settings)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-settings)

(defcfun "webkit_web_view_get_find_controller" (g-object webkit-find-controller)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-find-controller)

(defcfun "webkit_web_view_stop_loading" :void
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-stop-loading)

(defcfun "webkit_web_view_run_javascript" :void
  (web-view (g-object webkit-web-view))
  (script :string)
  (cancellable :pointer)
  (async-ready-callback :pointer)
  (user-data :pointer))
(export 'webkit-web-view-run-javascript)
