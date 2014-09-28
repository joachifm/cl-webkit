;;; webkit2.permission-request.lisp --- bindings for WebKitPermissionRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-g-interface "WebKitPermissionRequest" webkit-permission-request
  (:export t
   :type-initializer "webkit_permission_request_get_type"))

(defcfun "webkit_permission_request_allow" :void
  (permission-request (g-object webkit-permission-request)))
(export 'webkit-permission-request-allow)

(defcfun "webkit_permission_request_deny" :void
  (permission-request (g-object webkit-permission-request)))
(export 'webkit-permission-request-deny)
