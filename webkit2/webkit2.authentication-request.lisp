;;; webkit2.authentication-request.lisp --- bindings for WebKitAuthenticationRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitAuthenticationRequest" () ())

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

(defcfun "webkit_authentication_request_get_realm" :string
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-get-realm)

(defcfun "webkit_authentication_request_get_scheme" webkit-authentication-scheme
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-get-scheme)

(defcfun "webkit_authentication_request_is_proxy" :boolean
  (request (g-object webkit-authentication-request)))
(export 'webkit-authentication-request-is-proxy)
