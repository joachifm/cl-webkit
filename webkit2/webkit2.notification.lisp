;;; webkit2.notification.lisp --- bindings for WebKitNotification

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitNotification" ()
  (("body" "gchararray")
   ("id" "guint64")
   ("tag" "gchararray")
   ("title" "gchararray")))

(defcfun "webkit_notification_get_id" :long
  (notification (g-object webkit-notification)))
(export 'webkit-notification-get-id)

(defcfun "webkit_notification_get_title" :string
  (notification (g-object webkit-notification)))
(export 'webkit-notification-get-title)

(defcfun "webkit_notification_get_body" :string
  (notification (g-object webkit-notification)))
(export 'webkit-notification-get-body)

(defcfun "webkit_notification_get_tag" :string
  (notification (g-object webkit-notification)))
(export 'webkit-notification-get-tag)

(defcfun "webkit_notification_close" :void
  (notification (g-object webkit-notification)))
(export 'webkit-notification-close)

(defcfun "webkit_notification_clicked" :void
  (notification (g-object webkit-notification)))
(export 'webkit-notification-clicked)
