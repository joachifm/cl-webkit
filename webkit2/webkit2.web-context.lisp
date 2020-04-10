;;; webkit2.web-context.lisp --- bindings for WebKitWebContext

;; Copyright (c) 2014 Aaron France
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitWebContext" () ())

(define-g-enum "WebKitCacheModel" webkit-cache-model ()
  :webkit-cache-model-document-viewer
  :webkit-cache-model-web-browser
  :webkit-cache-model-document-browser)

(define-g-enum "WebKitTLSErrorsPolicy" webkit-tls-errors-policy ()
  :webkit-tls-errors-policy-ignore
  :webkit-tls-errors-policy-fail)

(define-g-enum "WebKitProcessModel" webkit-process-model ()
  :webkit-process-model-shared-secondary-process
  :webkit-process-model-multiple-secondary-processes)

(define-g-enum "WebKitNetworkProxyMode" webkit-network-proxy-mode ()
  :webkit-network-proxy-mode-default
  :webkit-network-proxy-mode-no-proxy
  :webkit-network-proxy-mode-custom)

(defcfun "webkit_web_context_get_default" (g-object webkit-web-context))
(export 'webkit-web-context-get-default)

(defcfun "webkit_web_context_set_cache_model" :void
  (webkit-web-context (g-object webkit-web-context))
  (webkit-cache-model webkit-cache-model))
(export 'webkit-web-context-set-cache-model)

(defcfun "webkit_web_context_get_cache_model" webkit-cache-model
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-get-cache-model)

(defcfun "webkit_web_context_clear_cache" :void
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-clear-cache)

(defcfun "webkit_web_context_download_uri" (g-object webkit-download)
  (webkit-web-context (g-object webkit-web-context))
  (uri :string))
(export 'webkit-web-context-download-uri)

(defcfun "webkit_web_context_get_cookie_manager" (g-object webkit-cookie-manager)
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-get-cookie-manager)

(defcfun "webkit_web_context_get_favicon_database" (g-object webkit-favicon-database)
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-get-favicon-database)

(defcfun "webkit_web_context_set_favicon_database_directory" :void
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-set-favicon-database-directory)

(defcfun "webkit_web_context_get_favicon_database_directory" :string
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-get-favicon-database-directory)

(defcfun "webkit_web_context_get_security_manager" (g-object webkit-security-manager)
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-get-security-manager)

(defcfun "webkit_web_context_set_additional_plugins_directory" :void
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-set-additional-plugins-directory)

(defcfun "webkit_web_context_get_plugins" :void
  (webkit-web-context (g-object webkit-web-context))
  (cancellable :pointer)
  (callback    :pointer)
  (user_data   :pointer))
(export 'webkit-web-context-get-plugins)

(defcfun ("webkit_web_context_get_plugins_finish" %webkit-web-context-get-plugins-finish) (glib:g-list webkit-plugin)
  (webkit-web-context (g-object webkit-web-context))
  (result g-async-result)
  (gerror :pointer))

(defun webkit-web-context-get-plugins-finish (web-context result)
  (glib:with-g-error (err)
    (%webkit-web-context-get-plugins-finish web-context result err)))

(export 'webkit-web-context-get-plugins-finish)

(defcfun "webkit_web_context_get_spell_checking_enabled" :boolean
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-get-spell-checking-enabled)

(defcfun "webkit_web_context_set_spell_checking_enabled" :void
  (webkit-web-context (g-object webkit-web-context))
  (enabled :boolean))
(export 'webkit-web-context-set-spell-checking-enabled)

(defcfun "webkit_web_context_get_spell_checking_languages" :pointer ;; XXX: const gchar * const *
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-get-spell-checking-languages)

(defcfun "webkit_web_context_set_spell_checking_languages" :void
  (webkit-web-context (g-object webkit-web-context))
  (languages :string))
(export 'webkit-web-context-set-spell-checking-languages)

(defcfun "webkit_web_context_set_preferred_languages" :void
  (webkit-web-context (g-object webkit-web-context))
  (languages :string))
(export 'webkit-web-context-set-preferred-languages)

(defcfun "webkit_web_context_set_tls_errors_policy" :void
  (webkit-web-context (g-object webkit-web-context))
  (policy webkit-tls-errors-policy))
(export 'webkit-web-context-set-tls-errors-policy)

(defcfun "webkit_web_context_get_tls_errors_policy" webkit-tls-errors-policy
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-get-tls-errors-policy)

(defcfun "webkit_web_context_allow_tls_certificate_for_host" :void
  (webkit-web-context (g-object webkit-web-context))
  (certificate :pointer) ; XXX: GTlsCertificate
  (host :string))
(export 'webkit-web-context-allow-tls-certificate-for-host)

(defcfun "webkit_web_context_set_web_extensions_directory" :void
  (webkit-web-context (g-object webkit-web-context))
  (directory :string))
(export 'webkit-web-context-set-web-extensions-directory)

(defcfun "webkit_web_context_set_web_extensions_initialization_user_data" :void
  (webkit-web-context (g-object webkit-web-context))
  (user_data :pointer))
(export 'webkit-web-context-set-web-extensions-initialization-user-data)

(defcfun "webkit_web_context_prefetch_dns" :void
  (webkit-web-context (g-object webkit-web-context))
  (hostname :string))
(export 'webkit-web-context-prefetch-dns)

(defcfun "webkit_web_context_set_disk_cache_directory" :void
  (webkit-web-context (g-object webkit-web-context))
  (hostname :string))
(export 'webkit-web-context-set-disk-cache-directory)

(defcfun "webkit_web_context_set_process_model" :void
  (webkit-web-context (g-object webkit-web-context))
  (webkit-process-model webkit-process-model))
(export 'webkit-web-context-set-process-model)

(defcfun "webkit_web_context_get_process_model" webkit-process-model
  (webkit-web-context (g-object webkit-web-context)))
(export 'webkit-web-context-get-process-model)

(defcfun "webkit_web_context_register_uri_scheme" :void
  (webkit-web-context (g-object webkit-web-context))
  (scheme :string)
  (webkit-uri-scheme-request-callback :pointer)
  (user_data :pointer)
  (gdestroynotify :pointer))
(export 'webkit-web-context-register-uri-scheme)

(defcfun "webkit_web_context_set_network_proxy_settings" :void
  (webkit-web-context (g-object webkit-web-context))
  (proxy-mode webkit-network-proxy-mode)
  (proxy-settings (g-object webkit-network-proxy-settings)))
(export 'webkit-web-context-set-network-proxy-settings)

#+webkit2-sandboxing
(progn
  (defcfun "webkit_web_context_get_sandbox_enabled" :boolean
    (webkit-web-context (g-object webkit-web-context)))
  (export 'webkit-web-context-get-sandbox-enabled)
  (defcfun "webkit_web_context_set_sandbox_enabled" :void
    (webkit-web-context (g-object webkit-web-context))
    (enabled :boolean))
  (export 'webkit-web-context-set-sandbox-enabled)
  (defcfun "webkit_web_context_add_path_to_sandbox" :void
    (webkit-web-context (g-object webkit-web-context))
    (path :string)
    (read-only :boolean))
  (export 'webkit-web-context-add-path-to-sandbox))
