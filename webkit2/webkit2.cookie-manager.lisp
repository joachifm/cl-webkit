;;; webkit2.cookie-manager.lisp --- bindings for WebKitCookieManager

;; Copyright (c) 2014 Aaron France
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(defclass webkit-cookie-manager (g-object)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "WebKitCookieManager")
  (:g-type-initializer . "webkit_cookie_manager_get_type"))

(export 'webkit-cookie-manager)

(defcenum webkit-cookie-persistent-storage
  :webkit-cookie-persistent-storage-text
  :webkit-cookie-persistent-storage-sqlite)
(export 'webkit-cookie-persistent-storage)

(defconstant +webkit-cookie-persistent-storage-text+
  (foreign-enum-value
   'webkit-cookie-persistent-storage :webkit-cookie-persistent-storage-text))

(export '+webkit-cookie-persistent-storage-text+)

(defconstant +webkit-cookie-persistent-storage-sqlite+
  (foreign-enum-value
   'webkit-cookie-persistent-storage :webkit-cookie-persistent-storage-sqlite))

(export '+webkit-cookie-persistent-storage-sqlite+)

(defcenum webkit-cookie-accept-policy
  :webkit-cookie-policy-accept-always
  :webkit-cookie-policy-accept-never
  :webkit-cookie-policy-accept-no-third-party)
(export 'webkit-cookie-accept-policy)

(defconstant +webkit-cookie-policy-accept-always+
  (foreign-enum-value
   'webkit-cookie-accept-policy :webkit-cookie-policy-accept-always))

(export '+webkit-cookie-policy-accept-always+)

(defconstant +webkit-cookie-policy-accept-never+
  (foreign-enum-value
   'webkit-cookie-accept-policy :webkit-cookie-policy-accept-never))

(export '+webkit-cookie-policy-accept-never+)

(defconstant +webkit-cookie-policy-accept-no-third-party+
  (foreign-enum-value
   'webkit-cookie-accept-policy :webkit-cookie-policy-accept-no-third-party))

(export '+webkit-cookie-policy-accept-no-third-party+)

(defcfun "webkit_cookie_manager_get_type" :void
  (webkit-cookie-manager (g-object webkit-cookie-manager)))
(export 'webkit-cookie-manager-get-type)

(defcfun "webkit_cookie_manager_set_persistent_storage" :void
  (webkit-cookie-manager (g-object webkit-cookie-manager))
  (filename :string)
  (storage webkit-cookie-persistent-storage))
(export 'webkit-cookie-manager-set-persistent-storage)

(defcfun "webkit_cookie_manager_set_accept_policy" :void
  (webkit-cookie-manager (g-object webkit-cookie-manager))
  (policy webkit-cookie-accept-policy))
(export 'webkit-cookie-manager-set-accept-policy)

(defcfun "webkit_cookie_manager_get_accept_policy" :void
  (webkit-cookie-manager (g-object webkit-cookie-manager))
  (cancellable :pointer)
  (callback    :pointer)
  (user_data   :pointer))
(export 'webkit-cookie-manager-get-accept-policy)

(defcfun webkit_cookie_manager_get_accept_policy_finish webkit-cookie-accept-policy
  (webkit-cookie-manager (g-object webkit-cookie-manager))
  (result :pointer)
  (error  :pointer))
(export 'webkit-cookie-manager-get-accept-policy-finish)

(defcfun "webkit_cookie_manager_get_domains_with_cookies" :void
  (webkit-cookie-manager (g-object webkit-cookie-manager))
  (cancellable :pointer)
  (callback    :pointer)
  (user_data   :pointer))
(export 'webkit-cookie-manager-get-domains-with-cookies)

(defcfun "webkit_cookie_manager_get_domains_with_cookies_finish" :string
  (webkit-cookie-manager (g-object webkit-cookie-manager))
  (result :pointer)
  (error  :pointer))
(export 'webkit-cookie-manager-get-domains-with-cookies-finish)

(defcfun "webkit_cookie_manager_delete_cookies_for_domain" :void
  (webkit-cookie-manager (g-object webkit-cookie-manager))
  (domain :string))
(export 'webkit-cookie-manager-delete-cookies-for-domain)

(defcfun "webkit_cookie_manager_delete_all_cookies" :void
  (webkit-cookie-manager (g-object webkit-cookie-manager)))
(export 'webkit_cookie_manager_delete_all_cookies)
