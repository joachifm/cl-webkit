;;; webkit2.settings.lisp --- bindings for WebKitSettings

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitSettings" ()
   (("allow-file-access-from-file-urls" "gboolean" t t)
    ("allow-modal-dialogs" "gboolean" t t)
    ("allow-universal-access-from-file-urls" "gboolean" t t)
    ("allow-top-navigation-to-data-urls" "gboolean" t t)
    ("auto-load-images" "gboolean" t t)
    ("cursive-font-family" "gchararray" t t)
    ("default-charset" "gchararray" t t)
    ("default-font-family" "gchararray" t t)
    ("default-font-size" "guint" t t)
    ("default-monospace-font-size" "guint" t t)
    ("draw-compositing-indicators" "gboolean" t t)
    ("enable-accelerated-2d-canvas" "gboolean" t t)
    ("enable-back-forward-navigation-gestures" "gboolean" t t)
    ("enable-caret-browsing" "gboolean" t t)
    ("enable-developer-extras" "gboolean" t t)
    ("enable-dns-prefetching" "gboolean" t t)
    ("enable-encrypted-media" "gboolean" t t)
    ("enable-frame-flattening" "gboolean" t t)
    ("enable-fullscreen" "gboolean" t t)
    ("enable-html5-database" "gboolean" t t)
    ("enable-html5-local-storage" "gboolean" t t)
    ("enable-hyperlink-auditing" "gboolean" t t)
    ("enable-java" "gboolean" t t)
    ("enable-javascript" "gboolean" t t)
    ("enable-javascript-markup" "gboolean" t t)
    ("enable-media-capabilities" "gboolean" t t)
    ("enable-media-stream" "gboolean" t t)
    ("enable-mediasource" "gboolean" t t)
    ("enable-mock-capture-devices" "gboolean" t t)
    ("enable-offline-web-application-cache" "gboolean" t t)
    ("enable-page-cache" "gboolean" t t)
    ("enable-plugins" "gboolean" t t)
    ("enable-private-browsing" "gboolean" t t) ;; Deprecated since 2.16
    ("enable-resizable-text-areas" "gboolean" t t)
    ("enable-site-specific-quirks" "gboolean" t t)
    ("enable-smooth-scrolling" "gboolean" t t)
    ("enable-spatial-navigation" "gboolean" t t)
    ("enable-tabs-to-links" "gboolean" t t)
    ("enable-webaudio" "gboolean" t t)
    ("enable-webgl" "gboolean" t t)
    ("enable-write-console-messages-to-stdout" "gboolean" t t)
    ("enable-xss-auditor" "gboolean" t t)
    ("fantasy-font-family" "gchararray" t t)
    ("javascript-can-access-clipboard" "gboolean" t t)
    ("javascript-can-open-windows-automatically" "gboolean" t t)
    ("load-icons-ignoring-image-load-setting" "gboolean" t t)
    ("media-playback-allows-inline" "gboolean" t t)
    ("media-playback-requires-user-gesture" "gboolean" t t)
    ("minimum-font-size" "guint" t t)
    ("monospace-font-family" "gchararray" t t)
    ("pictograph-font-family" "gchararray" t t)
    ("print-backgrounds" "gboolean" t t)
    ("sans-serif-font-family" "gchararray" t t)
    ("serif-font-family" "gchararray" t t)
    ("user-agent" "gchararray" t t)
    ("zoom-text-only" "gboolean" t t)
    #+webkit2-media
    ("enable-media" "gboolean" t t)))

(define-g-enum "WebKitHardwareAccelerationPolicy"
    webkit-hardware-acceleration-policy ()
  :webkit-hardware-acceleration-policy-on-demand
  :webkit-hardware-acceleration-policy-always
  :webkit-hardware-acceleration-policy-never)

(defcfun "webkit_settings_get_hardware_acceleration_policy"
    webkit-hardware-acceleration-policy
  (settings (g-object webkit-settings)))
(export 'webkit-settings-get-hardware-acceleration-policy)

(defcfun "webkit_settings_set_hardware_acceleration_policy"
    :void
  (settings (g-object webkit-settings))
  (policy webkit-hardware-acceleration-policy))
(export 'webkit-settings-set-hardware-acceleration-policy)

(defcfun "webkit_settings_font_size_to_points" :uint
  (pixels :uint))
(export 'webkit-settings-font-size-to-points)

(defcfun "webkit_settings_font_size_to_pixels" :uint
  (points :uint))
(export 'webkit-settings-font-size-to-pixels)
