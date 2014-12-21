;;; webkit2.window-properties.lisp --- bindings for WebKitWindowProperties

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitWindowProperties" ()
  (("fullscreen" "gboolean" t t)
   ("geometry" "GdkRectangle" t t)
   ("locationbar-visible" "gboolean" t t)
   ("menubar-visible" "gboolean" t t)
   ("resizable" "gboolean" t t)
   ("scrollbars-visible" "gboolean" t t)
   ("statusbar-visible" "gboolean" t t)
   ("toolbar-visible" "gboolean" t t)))
