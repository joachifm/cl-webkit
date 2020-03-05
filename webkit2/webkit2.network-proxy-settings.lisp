;;; webkit2.network-proxy-settings.lisp --- Bindings for WebKitNetworkProxySettings.

;; Copyright (c) 2020 Aaron France
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

;; See https://webkitgtk.org/reference/webkit2gtk/unstable/WebKitNetworkProxySettings.html

(in-package #:webkit2)

(define-webkit-class "WebKitNetworkProxySettings" () ())

(defcfun "webkit_network_proxy_settings_new" :pointer
  (default-proxy-uri :string)
  (ignore-hosts :pointer))              ; XXX: const gchar * const *
(export 'webkit-network-proxy-settings-new)

(defcfun "webkit_network_proxy_settings_copy" :pointer
  (proxy-settings (g-object webkit-network-proxy-settings)))
(export 'webkit-network-proxy-settings-copy)

(defcfun "webkit_network_proxy_settings_free" :void
  (proxy-settings (g-object webkit-network-proxy-settings)))
(export 'webkit-network-proxy-settings-free)

(defcfun "webkit_network_proxy_settings_add_proxy_for_scheme" :void
  (proxy-settings (g-object webkit-network-proxy-settings))
  (scheme :string)
  (proxy-uri :string))
(export 'webkit-network-proxy-settings-add-proxy-for-scheme)
