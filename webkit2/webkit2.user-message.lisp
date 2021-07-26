;;; webkit2.user-message.lisp --- bindings for WebKitUserMessage

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitUserMessage" ()
  (("fd-list" "GUnixFDList")
   ("name" "gchararray")
   ("parameters" "GVariant")))

(define-g-enum "WebKitUserMessageError" webkit-user-message-error ()
  :webkit-user-message-unhandled-message)

(defcfun "webkit_user_message_error_quark" glib:g-quark)

(defcfun "webkit_user_message_new" (g-object webkit-user-message)
  (name :string)  ; XXX: const char *
  (parameters (:pointer (:struct glib:g-variant))))
(export 'webkit-user-message-new)

(defcfun "webkit_user_message_new_with_fd_list" (g-object webkit-user-message)
  (name :string)  ; XXX: const char *
  (parameters (:pointer (:struct glib:g-variant)))
  (fd-list :pointer)) ; fd_list *
(export 'webkit-user-message-new-with-fd-list)

(defcfun "webkit_user_message_get_name" :string ; XXX: const char *
  (message (g-object webkit-user-message)))
(export 'webkit-user-message-get-name)

(defcfun "webkit_user_message_get_parameters" (:pointer (:struct glib:g-variant)) ; GVariant *
  (message (g-object webkit-user-message)))
(export 'webkit-user-message-get-parameters)

(defcfun "webkit_user_message_get_fd_list" :pointer ; GUnixFDList *
  (message (g-object webkit-user-message)))
(export 'webkit-user-message-get-fd-list)

(defcfun "webkit_user_message_send_reply" :void
  (message (g-object webkit-user-message))
  (reply (g-object webkit-user-message)))
(export 'webkit-user-message-send-reply)
