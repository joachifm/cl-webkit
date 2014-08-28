;;; webkit2.web-context.lisp --- bindings for WebKitWebContext

;; Copyright (c) 2014 Aaron France
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(defctype webkit-web-context :pointer)
(export 'webkit-web-context)

(defcenum webkit-cache-model
  :webkit-cache-model-document-viewer
  :webkit-cache-model-web-browser
  :webkit-cache-model-document-browser)

(export 'webkit-cache-model)

(defvar webkit-cache-model-document-viewer
  (foreign-enum-value 'webkit-cache-model :webkit-cache-model-document-viewer))

(defvar webkit-cache-model-web-browser
  (foreign-enum-value 'webkit-cache-model :webkit-cache-model-web-browser))

(defvar webkit-cache-model-document-browser
  (foreign-enum-value 'webkit-cache-model :webkit-cache-model-document-browser))

(defcfun "webkit_web_context_get_default" webkit-web-context)
(export 'webkit-web-context-get-default)

(defcfun "webkit_web_context_set_cache_model" :void
  (webkit-web-context webkit-web-context)
  (webkit-cache-model webkit-cache-model))
(export 'webkit-web-context-set-cache-model)

(defcfun "webkit_web_context_get_cache_model" webkit-cache-model
  (webkit-web-context webkit-web-context))
(export 'webkit-web-context-get-cache-model)

(defcfun "webkit_web_context_clear_cache" :void
  (webkit-web-context webkit-web-context))
(export 'webkit-web-context-clear-cache)

(defcfun "webkit_web_context_download_uri" webkit-download
  (webkit-web-context webkit-web-context)
  (uri :string))
(export 'webkit-web-context-download-uri)

(defcfun "webkit_web_context_get_cookie_manager" webkit-cookie-manager
  (webkit-web-context webkit-web-context))
(export 'webkit-web-context-get-cookie-manager)

;; TODO: Implement WebKitFaviconDatabase.h
;; (defcfun "webkit_web_context_get_favicon_database" webkit-favicon-database
;;   (webkit-web-context webkit-web-context))

(defcfun "webkit_web_context_set_favicon_database_directory" :void
  (webkit-web-context webkit-web-context))
(export 'webkit-web-context-set-favicon-database-directory)

(defcfun "webkit_web_context_get_favicon_database_directory" :string
  (webkit-web-context webkit-web-context))
(export 'webkit-web-context-get-favicon-database-directory)

;; TODO: Implement WebKitSecurityManager.h
;; (defcfun "webkit_web_context_get_security_manager" webkit-security-manager
;;   (webkit-web-context webkit-web-context))

(defcfun "webkit_web_context_set_additional_plugins_directory" :void
  (webkit-web-context webkit-web-context))
(export 'webkit-web-context-set-additional-plugins-directory)

(defcfun "webkit_web_context_get_plugins" :void
  (webkit-web-context webkit-web-context)
  (cancellable :pointer)
  (callback    :pointer)
  (user_data   :pointer))
(export 'webkit-web-context-get-plugins)

;; XXX: Implement proper return type glist
(defcfun "webkit_web_context_get_plugins_finish" :pointer ; glib:glist
  (webkit-web-context webkit-web-context)
  (gasync-result :pointer)
  (gerror :pointer))
(export 'webkit-web-context-get-plugins-finish)

(defcfun "webkit_web_context_register_uri_scheme" :void
  (webkit-web-context webkit-web-context)
  (scheme :string)
  (webkit-uri-scheme-request-callback :pointer)
  (user_data :pointer)
  (gdestroynotify :pointer))
(export 'webkit-web-context-register-uri-scheme)

(defcfun "webkit_web_context_get_spell_checking_enabled" :boolean
  (webkit-web-context webkit-web-context))
(export 'webkit-web-context-get-spell-checking-enabled)

(defcfun "webkit_web_context_set_spell_checking_enabled" :void
  (webkit-web-context webkit-web-context)
  (enabled :boolean))
(export 'webkit-web-context-set-spell-checking-enabled)

(defcfun "webkit_web_context_get_spell_checking_languages" :pointer
  (webkit-web-context webkit-web-context))
(export 'webkit-web-context-get-spell-checking-languages)

(defcfun "webkit_web_context_set_spell_checking_languages" :void
  (webkit-web-context webkit-web-context)
  (languages :string))
(export 'webkit-web-context-set-spell-checking-languages)

(defcfun "webkit_web_context_set_preferred_languages" :void
  (webkit-web-context webkit-web-context)
  (languages :string))
(export 'webkit-web-context-set-preferred-languages)

(defcenum webkit-tls-errors-policy
  :webkit-tls-errors-policy-ignore
  :webkit-tls-errors-policy-fail)
(export 'webkit-tls-errors-policy)

(defvar webkit-tls-errors-policy-ignore
  (foreign-enum-value 'webkit-tls-errors-policy :webkit-tls-errors-policy-ignore))

(defvar webkit-tls-errors-policy-fail
  (foreign-enum-value 'webkit-tls-errors-policy :webkit-tls-errors-policy-fail))

(defcfun "webkit_web_context_set_tls_errors_policy" :void
  (webkit-web-context webkit-web-context)
  (policy webkit-tls-errors-policy))
(export 'webkit-web-context-set-tls-errors-policy)

(defcfun "webkit_web_context_get_tls_errors_policy" webkit-tls-errors-policy
  (webkit-web-context webkit-web-context))
(export 'webkit-web-context-get-tls-errors-policy)

(defcfun "webkit_web_context_set_web_extensions_directory" :void
  (webkit-web-context webkit-web-context)
  (directory :string))
(export 'webkit-web-context-set-web-extensions-directory)

(defcfun "webkit_web_context_set_web_extensions_initialization_user_data" :void
  (webkit-web-context webkit-web-context)
  (user_data :pointer))
(export 'webkit-web-context-set-web-extensions-initialization-user-data)

(defcfun "webkit_web_context_prefetch_dns" :void
  (webkit-web-context webkit-web-context)
  (hostname :string))
(export 'webkit-web-context-prefetch-dns)

(defcfun "webkit_web_context_set_disk_cache_directory" :void
  (webkit-web-context webkit-web-context)
  (hostname :string))
(export 'webkit-web-context-set-disk-cache-directory)

(defcfun "webkit_web_context_allow_tls_certificate_for_host" :void
  (webkit-web-context webkit-web-context)
  (webkit-certificate-info :pointer)
  (host :string))
(export 'webkit-web-context-allow-tls-certificate-for-host)

(defcfun "webkit_web_context_allow_tls_certificate_for_host" :void
  (webkit-web-context webkit-web-context)
  (webkit-certificate-info :pointer)
  (host :string))
(export 'webkit-web-context-allow-tls-certificate-for-host)

(defcenum webkit-process-model
  :webkit-process-model-shared-secondary-process
  :webkit-process-model-multiple-secondary-processes)
(export 'webkit-process-model)

(defvar webkit-process-model-shared-secondary-process
  (foreign-enum-value
   'webkit-process-model :webkit-process-model-shared-secondary-process))

(defvar webkit-process-model-multiple-secondary-processes
  (foreign-enum-value
   'webkit-process-model :webkit-process-model-multiple-secondary-processes))

(defcfun "webkit_web_context_set_process_model" :void
  (webkit-web-context webkit-web-context)
  (webkit-process-model webkit-process-model))
(export 'webkit-web-context-set-process-model)

(defcfun "webkit_web_context_get_process_model" webkit-process-model
  (webkit-web-context webkit-web-context))
(export 'webkit_web_context_get_process_model)
