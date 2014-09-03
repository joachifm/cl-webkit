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

(defclass webkit-web-view-class (gtk-widget)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "WebKitWebView"))

(export 'webkit-web-view-class)

(defctype webkit-web-view :pointer)
(export 'webkit-web-view)

(define-g-enum "WebKitLoadEvent" webkit-load-event ()
  :webkit-load-started
  :webkit-load-redirected
  :webkit-load-committed
  :webkit-load-finished)

(defcfun "webkit_web_view_new" (g-object webkit-web-view-class))
(export 'webkit-web-view-new)

(defcfun "webkit_web_view_new_with_context" (g-object webkit-web-view-class)
  (webkit-web-context (g-object webkit-web-context-class)))
(export 'webkit-web-view-new-with-context)

(defcfun "webkit_web_view_load_uri" :void
  (web-view (g-object webkit-web-view-class))
  (uri :string))
(export 'webkit-web-view-load-uri)

(defcfun "webkit_web_view_load_html" :void
  (web-view (g-object webkit-web-view-class))
  (content :string)
  (base-uri :string))
(export 'webkit-web-view-load-html)

(defcfun "webkit_web_view_go_back" :void
  (web-view (g-object webkit-web-view-class)))
(export 'webkit-web-view-go-back)

(defcfun "webkit_web_view_go_forward" :void
  (web-view (g-object webkit-web-view-class)))
(export 'webkit-web-view-go-forward)

(defcfun "webkit_web_view_reload" :void
  (web-view (g-object webkit-web-view-class)))
(export 'webkit-web-view-reload)

(defcfun "webkit_web_view_set_zoom_level" :void
  (web-view (g-object webkit-web-view-class))
  (zoom-level :double))
(export 'webkit-web-view-set-zoom-level)

(defcfun "webkit_web_view_get_zoom_level" :double
  (web-view (g-object webkit-web-view-class)))
(export 'webkit-web-view-get-zoom-level)

(defcfun "webkit_web_view_get_find_controller" (g-object webkit-find-controller-class)
  (web-view (g-object webkit-web-view-class)))
(export 'webkit-web-view-get-find-controller)

(defcfun "webkit_web_view_stop_loading" :void
  (web-view (g-object webkit-web-view-class)))
(export 'webkit-web-view-stop-loading)

(defcfun "webkit_web_view_run_javascript" :void
  (web-view (g-object webkit-web-view-class))
  (script :string)
  (cancellable :pointer)
  (async-ready-callback :pointer)
  (user-data :pointer))
(export 'webkit-web-view-run-javascript)
