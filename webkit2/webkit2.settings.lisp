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

(define-g-object-class "WebKitSettings" webkit-settings
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "webkit_settings_get_type")
   ((allow-modal-dialogs-p settings-allow-modal-dialogs-p "allow-modal-dialogs" "gboolean" t t)
    (auto-load-images-p settings-auto-load-images-p "auto-load-images" "gboolean" t t)
    (cursive-font-family settings-cursive-font-family "cursive-font-family" "gchararray" t t)
    (default-charset settings-default-charset "default-charset" "gchararray" t t)
    (default-font-family settings-default-font-family "default-font-family" "gchararray" t t)
    (default-font-size settings-default-font-size "default-font-size" "guint" t t)
    (default-monospace-font-size settings-default-monospace-font-size "default-monospace-font-size" "guint" t t)
    (enable-caret-browsing-p settings-enable-caret-browsing-p "enable-caret-browsing" "gboolean" t t)
    (enable-dns-prefetching-p settings-enable-dns-prefetching-p "enable-dns-prefetching" "gboolean" t t)
    (enable-fullscreen-p settings-enable-fullscreen-p "enable-fullscreen" "gboolean" t t)
    (enable-html5-database-p settings-enable-html5-database-p "enable-html5-database" "gboolean" t t)
    (enable-html5-local-storage-p settings-enable-html5-local-storage-p "enable-html5-local-storage" "gboolean" t t)
    (enable-hyperlink-auditing-p settings-enable-hyperlink-auditing-p "enable-hyperlink-auditing" "gboolean" t t)
    (enable-java-p settings-enable-java-p "enable-java" "gboolean" t t)
    (enable-javascript-p settings-enable-javascript-p "enable-javascript" "gboolean" t t)
    (enable-page-cache-p settings-enable-page-cache-p "enable-page-cache" "gboolean" t t)
    (enable-plugins-p settings-enable-plugins-p "enable-plugins" "gboolean" t t)
    (enable-private-browsing-p settings-enable-private-browsing-p "enable-private-browsing" "gboolean" t t)
    (enable-resizable-text-areas-p settings-enable-resizable-text-areas-p "enable-resizable-text-areas" "gboolean" t t)
    (enable-webaudio-p settings-enable-webaudio-p "enable-webaudio" "gboolean" t t)
    (enable-webgl-p settings-enable-webgl-p "enable-webgl" "gboolean" t t)
    (enable-xss-auditor-p settings-enable-xss-auditor-p "enable-xss-auditor" "gboolean" t t)
    (javascript-can-access-clipboard-p settings-javascript-can-access-clipboard-p "javascript-can-access-clipboard" "gboolean" t t)
    (javascript-can-open-windows-automatically-p settings-javascript-can-open-windows-automatically-p
                                                 "javascript-can-open-windows-automatically" "gboolean" t t)
    (minimum-font-size settings-minimum-font-size "minimum-font-size" "guint" t t)
    (monospace-font-family settings-monospace-font-family "monospace-font-family" "gchararray" t t)
    (sans-serif-font-family settings-sans-serif-font-family "sans-serif-font-family" "gchararray" t t)
    (serif-font-family settings-serif-font-family "serif-font-family" "gchararray" t t)
    (user-agent settings-user-agent "user-agent" "gchararray" t t)
    (zoom-text-only-p settings-zoom-text-only-p "zoom-text-only" "gboolean" t t)))

