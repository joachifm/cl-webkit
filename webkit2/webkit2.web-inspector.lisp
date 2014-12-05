;;; webkit2.web-inspector.lisp --- bindings for WebKitWebInspector

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitWebInspector" ()
  (("attached-height" "guint")
   ("inspected-uri" "gchararray")))

(defcfun "webkit_web_inspector_get_web_view" (g-object webkit-web-view)
  (inspector (g-object webkit-web-inspector)))
(export 'webkit-web-inspector-get-web-view)

(defcfun "webkit_web_inspector_is_attached" :boolean
  (inspector (g-object webkit-web-inspector)))
(export 'webkit-web-inspector-is-attached)

(defcfun "webkit_web_inspector_attach" :void
  (inspector (g-object webkit-web-inspector)))
(export 'webkit-web-inspector-attach)

(defcfun "webkit_web_inspector_detach" :void
  (inspector (g-object webkit-web-inspector)))
(export 'webkit-web-inspector-detach)

(defcfun "webkit_web_inspector_show" :void
  (inspector (g-object webkit-web-inspector)))
(export 'webkit-web-inspector-show)

(defcfun "webkit_web_inspector_close" :void
  (inspector (g-object webkit-web-inspector)))
(export 'webkit-web-inspector-close)
