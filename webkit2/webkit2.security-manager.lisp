;;; webkit2.security-manager.lisp --- bindings for WebKitSecurityManager

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitSecurityManager" () ())

(defcfun "webkit_security_manager_register_uri_scheme_as_local" :void
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-register-uri-scheme-as-local)

(defcfun "webkit_security_manager_uri_scheme_is_local" :boolean
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-uri-scheme-is-local)

(defcfun "webkit_security_manager_register_uri_scheme_as_no_access" :void
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-register-uri-scheme-as-no-access)

(defcfun "webkit_security_manager_uri_scheme_is_no_access" :boolean
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-uri-scheme-is-no-access)

(defcfun "webkit_security_manager_register_uri_scheme_as_display_isolated" :void
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-register-uri-scheme-as-display-isolated)

(defcfun "webkit_security_manager_uri_scheme_is_display_isolated" :boolean
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-uri-scheme-is-display-isolated)

(defcfun "webkit_security_manager_register_uri_scheme_as_secure" :void
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-register-uri-scheme-as-secure)

(defcfun "webkit_security_manager_uri_scheme_is_secure" :boolean
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-uri-scheme-is-secure)

(defcfun "webkit_security_manager_register_uri_scheme_as_cors_enabled" :void
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-register-uri-scheme-as-cors-enabled)

(defcfun "webkit_security_manager_uri_scheme_is_cors_enabled" :boolean
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-uri-scheme-is-cors-enabled)

(defcfun "webkit_security_manager_register_uri_scheme_as_empty_document" :void
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-register-uri-scheme-as-empty-document)

(defcfun "webkit_security_manager_uri_scheme_is_empty_document" :boolean
  (security-manager (g-object webkit-security-manager))
  (scheme :string))
(export 'webkit-security-manager-uri-scheme-is-empty-document)
