;;; webkit2.authentication-request.lisp --- bindings for WebKitAuthenticationRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitAuthenticationRequest" () ())

(defctype webkit-credential :pointer) ; XXX: GBoxed struct WebKitCredential

(define-g-enum "WebKitAuthenticationScheme" webkit-authentication-scheme ()
  :webkit-authentication-scheme-default
  :webkit-authentication-scheme-http-basic
  :webkit-authentication-scheme-http-digest
  :webkit-authentication-scheme-html-form
  :webkit-authentication-scheme-ntlm
  :webkit-authentication-scheme-negotiate
  :webkit-authentication-scheme-client-certificate-requested
  :webkit-authentication-scheme-server-trust-evaluation-requested
  :webkit-authentication-scheme-unknown)

(define-g-enum "WebKitCredentialPersistence" webkit-credential-persistence ()
  :webkit-credential-persistence-none
  :webkit-credential-persistence-for-session
  :webkit-credential-persistence-permanent)

(defcfun "webkit_authentication_request_authenticate" :void
  (request (g-object webkit-authentication-request))
  (credential webkit-credential))
(export 'webkit-authentication-request-authenticate)

(defcfun "webkit_authentication_request_cancel" :void
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-cancel)

(defcfun "webkit_authentication_request_can_save_credentials" :boolean
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-can-save-credentials)

(defcfun "webkit_authentication_request_get_host" :string
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-get-host)

(defcfun "webkit_authentication_request_get_port" :string
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-get-port)

(defcfun "webkit_authentication_request_is_retry" :boolean
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-is-retry)

(defcfun "webkit_authentication_request_get_proposed_credential" webkit-credential
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-get-proposed-credential)

(defcfun "webkit_authentication_request_get_realm" :string
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-get-realm)

(defcfun "webkit_authentication_request_get_scheme" webkit-authentication-scheme
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-get-scheme)

(defcfun "webkit_authentication_request_is_for_proxy" :boolean
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-is-for-proxy)

(defcfun "webkit_credential_new" webkit-credential
  (username :string)
  (password :string)
  (persistence webkit-credential-persistence))
(export 'webkit-credential-new)

(defcfun "webkit_credential_copy" webkit-credential
  (credential webkit-credential))
(export 'webkit-credential-copy)

(defcfun "webkit_credential_free" :void
  (credential webkit-credential))
(export 'webkit-credential-free)

(defcfun "webkit_credential_get_password" :string
  (credential webkit-credential))
(export 'webkit-credential-get-password)

(defcfun "webkit_credential_get_persistence" webkit-credential-persistence
  (credential webkit-credential))
(export 'webkit-credential-get-persistence)

(defcfun "webkit_credential_get_username" :string
  (credential webkit-credential))
(export 'webkit-credential-get-username)

(defcfun "webkit_credential_has_password" :boolean
  (credential webkit-credential))
(export 'webkit-credential-has-password)
