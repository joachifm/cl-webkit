;;; webkit2.web-view-group.lisp --- bindings for WebKitWebViewGroup

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitWebViewGroup" ()
  (("settings" "WebKitSettings" t t)))

(define-g-enum "WebKitInjectedContentFrames" webkit-injected-content-frames ()
  :webkit-injected-content-frames-all
  :webkit-injected-content-frames-top-only)

(defcfun "webkit_web_view_group_new" (g-object webkit-web-view-group)
  (name :string))
(export 'webkit-web-view-group-new)

(defcfun "webkit_web_view_group_get_name" :string
  (group (g-object webkit-web-view-group)))
(export 'webkit-web-view-group-get-name)
