;;; webkit2.web-extension.lisp --- bindings for WebKitWebExtension

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(defctype webkit-web-extension :pointer)

(defcfun "webkit_web_extension_get_page" (g-object webkit-web-page)
  (extension webkit-web-extension)
  (page-id :uint))
(export 'webkit-web-extension-get-page)

(defcfun "webkit_web_extension_send_message_to_context" :void
  (extension webkit-web-extension)
  (message (g-object webkit-user-message))
  (cancellable :pointer)  ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-web-extension-send-message-to-context)

(defcfun "webkit_web_extension_send_message_to_context_finish" (g-object webkit-user-message)
  (extension webkit-web-extension)
  (result g-async-result)
  (error (:pointer (:struct glib:g-error))))
(export 'webkit-web-extension-send-message-to-context-finish)
