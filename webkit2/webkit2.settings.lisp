;;; webkit2.settings.lisp --- bindings for WebKitSettings

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-g-object-class* "WebKitSettings" webkit-settings
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "webkit_settings_get_type")
   (("allow-modal-dialogs" "gboolean" t t)
    ("auto-load-images" "gboolean" t t)
    ("cursive-font-family" "gchararray" t t)
    ("default-charset" "gchararray" t t)
    ("default-font-family" "gchararray" t t)
    ("default-font-size" "guint" t t)
    ("default-monospace-font-size" "guint" t t)
    ("enable-caret-browsing" "gboolean" t t)
    ("enable-dns-prefetching" "gboolean" t t)
    ("enable-fullscreen" "gboolean" t t)
    ("enable-html5-database" "gboolean" t t)
    ("enable-html5-local-storage" "gboolean" t t)
    ("enable-hyperlink-auditing" "gboolean" t t)
    ("enable-java" "gboolean" t t)
    ("enable-javascript" "gboolean" t t)
    ("enable-page-cache" "gboolean" t t)
    ("enable-plugins" "gboolean" t t)
    ("enable-private-browsing" "gboolean" t t)
    ("enable-resizable-text-areas" "gboolean" t t)
    ("enable-webaudio" "gboolean" t t)
    ("enable-webgl" "gboolean" t t)
    ("enable-xss-auditor" "gboolean" t t)
    ("javascript-can-access-clipboard" "gboolean" t t)
    ("javascript-can-open-windows-automatically" "gboolean" t t)
    ("minimum-font-size" "guint" t t)
    ("monospace-font-family" "gchararray" t t)
    ("sans-serif-font-family" "gchararray" t t)
    ("serif-font-family" "gchararray" t t)
    ("user-agent" "gchararray" t t)
    ("zoom-text-only" "gboolean" t t)))

