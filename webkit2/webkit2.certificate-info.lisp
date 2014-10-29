;;; webkit2.certificate-info.lisp --- bindings for WebKitCertificateInfo

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(defctype webkit-certificate-info :pointer) ; GBoxed WebKitCertificateInfo

(defcfun "webkit_certificate_info_copy" webkit-certificate-info
  (info webkit-certificate-info))
(export 'webkit-certificate-info-copy)

(defcfun "webkit_certificate_info_free" webkit-certificate-info
  (info webkit-certificate-info))
(export 'webkit-certificate-info-free)

(defcfun "webkit_certificate_info_get_tls_certificate" :pointer ; XXX: GTlsCertificate
  (info webkit-certificate-info))
(export 'webkit-certificate-info-get-tls-certificate)

(defcfun "webkit_certificate_info_get_tls_errors" :uint ; XXX: GTlsCertificateFlags
  (info webkit-certificate-info))
(export 'webkit-certificate-info-get-tls-errors)
