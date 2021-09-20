;;; webkit2.permission-request.lisp --- bindings for WebKitPermissionRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-g-interface "WebKitPermissionRequest" webkit-permission-request
  (:export t
   :type-initializer "webkit_permission_request_get_type"))

(defcfun "webkit_permission_request_allow" :void
  (permission-request (g-object webkit-permission-request)))
(export 'webkit-permission-request-allow)

(defcfun "webkit_permission_request_deny" :void
  (permission-request (g-object webkit-permission-request)))
(export 'webkit-permission-request-deny)

(define-webkit-class "WebKitGeolocationPermissionRequest"
    (:interfaces ("WebKitPermissionRequest")) ())

(define-webkit-class "WebKitInstallMissingMediaPluginsPermissionRequest"
    (:interfaces ("WebKitPermissionRequest")) ())

(defcfun "webkit_install_missing_media_plugins_permission_request_get_description" :string
  (request (g-object webkit-install-media-plugins-permission-request)))
(export 'webkit-install-missing-media-plugins-permission-request-get-description)

(define-webkit-class "WebKitMediaKeySystemPermissionRequest"
    (:interfaces ("WebKitPermissionRequest")) ())

(defcfun "webkit_media_key_system_permission_get_name" :string
  (request (g-object webkit-media-key-system-permission-request)))
(export 'webkit-media-key-system-permission-get-name)

(define-webkit-class "WebKitUserMediaPermissionRequest"
    (:interfaces ("WebKitPermissionRequest"))
    (("is-for-audio-device" "gboolean")
     ("is-for-video-device" "gboolean")))

(defcfun "webkit_user_media_permission_is_for_audio_device" :boolean
  (request (g-object webkit-user-media-permission-request)))
(export 'webkit-user-media-permission-is-for-audio-device)

(defcfun "webkit_user_media_permission_is_for_video_device" :boolean
  (request (g-object webkit-user-media-permission-request)))
(export 'webkit-user-media-permission-is-for-video-device)

(define-webkit-class "WebKitDeviceInfoPermissionRequest"
    (:interfaces ("WebKitPermissionRequest")) ())

(define-webkit-class "WebKitNotificationPermissionRequest"
    (:interfaces ("WebKitPermissionRequest")) ())

(define-webkit-class "WebKitPointerLockPermissionRequest"
    (:interfaces ("WebKitPermissionRequest")) ())

(define-webkit-class "WebKitWebsiteDataAccessPermissionRequest"
    (:interfaces ("WebKitPermissionRequest")) ())

(defcfun "webkit_website_data_access_permission_request_get_requesting_domain" :string
  (request (g-object webkit-website-data-access-permission-request)))
(export 'webkit-website-data-access-permission-request-get-requesting-domain)

(defcfun "webkit_website_data_access_permission_request_get_current_domain" :string
  (request (g-object webkit-website-data-access-permission-request)))
(export 'webkit-website-data-access-permission-request-get-current-domain)
