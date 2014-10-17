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

(define-webkit-class "WebKitWebView"
  (:superclass gtk-widget
   :interfaces ("AtkImplementorIface" "GtkBuildable"))
  (("estimated-load-progress" "gdouble")
   ("is-loading" "gboolean")
   ("title" "gchararray")
   ("uri" "gchararray")
   ("view-mode" "WebKitViewMode" t t)
   ("web-context" "WebKitWebContext" t t)
   ("zoom-level" "gdouble" t t)))

(define-g-enum "WebKitLoadEvent" webkit-load-event ()
  :webkit-load-started
  :webkit-load-redirected
  :webkit-load-committed
  :webkit-load-finished)

(define-g-enum "WebKitInsecureContentEvent" webkit-insecure-content-event ()
  :webkit-insecure-content-run
  :webkit-insecure-content-displayed)

(define-g-enum "WebKitPolicyDecisionType" webkit-policy-decision-type ()
  :webkit-policy-decision-type-navigation-action
  :webkit-policy-decision-type-new-window-action
  :webkit-policy-decision-type-response)

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

(defcfun "webkit_web_view_download_uri" (g-object webkit-download)
  (web-view (g-object webkit-web-view))
  (uri :string))
(export 'webkit-web-view-download-uri)

(defcfun "webkit_web_view_get_main_resource" (g-object webkit-web-resource)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-main-resource)

(defcfun "webkit_web_view_get_back_forward_list" (g-object webkit-back-forward-list)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-back-forward-list)

(defcfun "webkit_web_view_can_go_back" :boolean
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-can-go-back)

(defcfun "webkit_web_view_can_go_forward" :boolean
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-can-go-forward)

(defcfun "webkit_web_view_can_show_mime_type" :boolean
  (web-view (g-object webkit-web-view))
  (mime-type :string))
(export 'webkit-web-view-can-show-mime-type)

(defcfun "webkit_web_view_reload_bypass_cache" :void
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-reload-bypass-cache)
