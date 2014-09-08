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

(defclass webkit-settings (g-object)
  (
   (allow-modal-dialogs-p
    :allocation :gobject-property
    :g-property-name "allow-modal-dialogs"
    :g-property-type :boolean
    :accessor settings-allow-modal-dialogs-p
    :initarg :allow-modal-dialogs-p)
   (auto-load-images-p
    :allocation :gobject-property
    :g-property-name "auto-load-images"
    :g-property-type :boolean
    :accessor settings-auto-load-images-p
    :initarg :auto-load-images-p)
   (cursive-font-family
    :allocation :gobject-property
    :g-property-name "cursive-font-family"
    :g-property-type :string
    :accessor settings-cursive-font-family
    :initarg :cursive-font-family)
   (default-charset
    :allocation :gobject-property
    :g-property-name "default-charset"
    :g-property-type :string
    :accessor settings-default-charset
    :initarg :default-charset)
   (default-font-family
    :allocation :gobject-property
     :g-property-name "default-font-family"
     :g-property-type :string
     :accessor settings-default-font-family
     :initarg :default-font-family)
   (default-font-size
    :allocation :gobject-property
    :g-property-name "default-font-size"
    :g-property-type :int
    :accessor settings-default-font-size
    :initarg :default-font-size)
   (default-monospace-font-size
    :allocation :gobject-property
    :g-property-name "default-monospace-font-size"
    :g-property-type :int
    :accessor settings-default-monospace-font-size
    :initarg :default-monospace-font-size)
   (enable-caret-browsing-p
    :allocation :gobject-property
    :g-property-name "enable-caret-browsing"
    :g-property-type :boolean
    :accessor settings-enable-caret-browsing-p
    :initarg :enable-caret-browsing-p)
   (enable-dns-prefetching-p
    :allocation :gobject-property
    :g-property-name "enable-dns-prefetching"
    :g-property-type :boolean
    :accessor settings-enable-dns-prefetching-p
    :initarg :enable-dns-prefetching-p)
   (enable-fullscreen-p
    :allocation :gobject-property
    :g-property-name "enable-fullscreen"
    :g-property-type :boolean
    :accessor settings-enable-fullscreen-p
    :initarg :enable-fullscreen-p)
   (enable-html5-database-p
    :allocation :gobject-property
    :g-property-name "enable-html5-database"
    :g-property-type :boolean
    :accessor settings-enable-html5-database-p
    :initarg :enable-html5-database-p)
   (enable-html5-local-storage-p
    :allocation :gobject-property
    :g-property-name "enable-html5-local-storage"
    :g-property-type :boolean
    :accessor settings-enable-html5-local-storage-p
    :initarg :enable-html5-local-storage-p)
   (enable-hyperlink-auditing-p
    :allocation :gobject-property
    :g-property-name "enable-hyperlink-auditing"
    :g-property-type :boolean
    :accessor settings-enable-hyperlink-auditing-p
    :initarg :enable-hyperlink-auditing-p)
   (enable-java-p
    :allocation :gobject-property
    :g-property-name "enable-java"
    :g-property-type :boolean
    :accessor settings-enable-java-p
    :initarg :enable-java-p)
   (enable-javascript-p
    :allocation :gobject-property
    :g-property-name "enable-javascript"
    :g-property-type :boolean
    :accessor settings-enable-javascript-p
    :initarg :enable-javascript-p)
   (enable-page-cache-p
    :allocation :gobject-property
    :g-property-name "enable-page-cache"
    :g-property-type :boolean
    :accessor settings-enable-page-cache-p
    :initarg :enable-page-cache-p)
   (enable-plugins-p
    :allocation :gobject-property
    :g-property-name "enable-plugins"
    :g-property-type :boolean
    :accessor settings-enable-plugins-p
    :initarg :enable-plugins-p)
   (enable-private-browsing-p
    :allocation :gobject-property
    :g-property-name "enable-private-browsing"
    :g-property-type :boolean
    :accessor settings-enable-private-browsing-p
    :initarg :enable-private-browsing-p)
   (enable-resizable-text-areas-p
    :allocation :gobject-property
    :g-property-name "enable-resizable-text-areas"
    :g-property-type :boolean
    :accessor settings-enable-resizable-text-areas-p
    :initarg :enable-resizable-text-areas-p)
   (enable-webaudio-p
    :allocation :gobject-property
    :g-property-name "enable-webaudio"
    :g-property-type :boolean
    :accessor settings-enable-webaudio-p
    :initarg :enable-webaudio-p)
   (enable-webgl-p
    :allocation :gobject-property
    :g-property-name "enable-webgl"
    :g-property-type :boolean
    :accessor settings-enable-webgl-p
    :initarg :enable-webgl-p)
   (enable-xss-auditor-p
    :allocation :gobject-property
    :g-property-name "enable-xss-auditor"
    :g-property-type :boolean
    :accessor settings-enable-xss-auditor-p
    :initarg :enable-xss-auditor-p)
   (javascript-can-access-clipboard-p
    :allocation :gobject-property
    :g-property-name "javascript-can-access-clipboard"
    :g-property-type :boolean
    :accessor settings-javascript-can-access-clipboard-p
    :initarg :enable-javascript-can-access-clipboard-p)
   (javascript-can-open-windows-automatically-p
    :allocation :gobject-property
    :g-property-name "javascript-can-open-windows-automatically"
    :g-property-type :boolean
    :accessor settings-javascript-can-open-windows-automatically-p
    :initarg :enable-javascript-can-open-windows-automatically-p)
   (minimum-font-size
    :allocation :gobject-property
    :g-property-name "minimum-font-size"
    :g-property-type :int
    :accessor settings-minimum-font-size
    :initarg :minimum-font-size)
   (monospace-font-family
    :allocation :gobject-property
    :g-property-name "monospace-font-family"
    :g-property-type :string
    :accessor settings-monospace-font-family
    :initarg :monospace-font-family)
   (sans-serif-font-family
    :allocation :gobject-property
    :g-property-name "sans-serif-font-family"
    :g-property-type :string
    :accessor settings-sans-serif-font-family
    :initarg :sans-serif-font-family)
   (serif-font-family
    :allocation :gobject-property
    :g-property-name "serif-font-family"
    :g-property-type :string
    :accessor settings-serif-font-family
    :initarg :serif-font-family)
   (user-agent
    :allocation :gobject-property
    :g-property-name "user-agent"
    :g-property-type :string
    :accessor settings-user-agent
    :initarg :user-agent)
   (zoom-text-only-p
    :allocation :gobject-property
    :g-property-name "zoom-text-only"
    :g-property-type :boolean
    :accessor settings-zoom-text-only-p
    :initarg :zoom-text-only-p)
   )
  (:metaclass gobject-class)
  (:g-type-name . "WebKitSettings")
  (:g-type-initializer . "webkit_settings_get_type"))

(export 'settings-allow-modal-dialogs-p)
(export 'settings-auto-load-images-p)
(export 'settings-cursive-font-family)
(export 'settings-default-charset)
(export 'settings-default-font-family)
(export 'settings-default-font-size)
(export 'settings-default-monospace-font-size)
(export 'settings-enable-caret-browsing-p)
(export 'settings-enable-dns-prefetching-p)
(export 'settings-enable-fullscreen-p)
(export 'settings-enable-html5-database-p)
(export 'settings-enable-html5-local-storage-p)
(export 'settings-enable-hyperlink-auditing-p)
(export 'settings-enable-java-p)
(export 'settings-enable-javascript-p)
(export 'settings-enable-page-cache-p)
(export 'settings-enable-plugins-p)
(export 'settings-enable-private-browsing-p)
(export 'settings-enable-resizable-text-areas-p)
(export 'settings-enable-webaudio-p)
(export 'settings-enable-webgl-p)
(export 'settings-enable-xss-auditor-p)
(export 'settings-javascript-can-access-clipboard-p)
(export 'settings-javascript-can-open-windows-automatically-p)
(export 'settings-minimum-font-size)
(export 'settings-monospace-font-family)
(export 'settings-sans-serif-font-family)
(export 'settings-serif-font-family)
(export 'settings-user-agent)
(export 'settings-zoom-text-only-p)
