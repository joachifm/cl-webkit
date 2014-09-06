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
   (default-charset
    :allocation :gobject-property
    :g-property-name "default-charset"
    :g-property-type :string
    :accessor settings-default-charset
    :initarg :default-charset)
   (default-font-size
    :allocation :gobject-property
    :g-property-name "default-font-size"
    :g-property-type :int
    :accessor settings-default-font-size
    :initarg :default-font-size)
   (enable-dns-prefetching-p
    :allocation :gobject-property
    :g-property-name "enable-dns-prefetching"
    :g-property-type :boolean
    :accessor settings-enable-dns-prefetching-p
    :initarg :enable-dns-prefetching-p)
   (enable-javascript-p
    :allocation :gobject-property
    :g-property-name "enable-javascript"
    :g-property-type :boolean
    :accessor settings-enable-javascript-p
    :initarg :enable-javascript-p)
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
   (enable-xss-auditor-p
    :allocation :gobject-property
    :g-property-name "enable-xss-auditor"
    :g-property-type :boolean
    :accessor settings-enable-xss-auditor-p
    :initarg :enable-xss-auditor-p)
   (user-agent
    :allocation :gobject-property
    :g-property-name "user-agent"
    :g-property-type :string
    :accessor settings-user-agent
    :initarg :user-agent)
   )
  (:metaclass gobject-class)
  (:g-type-name . "WebKitSettings")
  (:g-type-initializer . "webkit_settings_get_type"))
