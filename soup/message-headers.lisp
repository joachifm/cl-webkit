;;;; message-headers.lisp -- binds SoupMessageHeaders

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:cl-soup)

(defctype soup-message-headers :pointer)

(export 'soup-message-headers)

(defcfun "soup_message_headers_get_one" :string
  (headers soup-message-headers)
  (name :string))

(defcfun "soup_message_headers_get_list" :string
  (headers soup-message-headers)
  (name :string))

;;; libsoup provides a deprecated function of the same name; users
;;; of this binding are unlikely to care whether the header they are
;;; getting can occur only one, so provide a unified interface.
;;;
;;; Ideally, we'd have a soup-message-headers->hashtable function.
(defun soup-message-headers-get (headers name)
  (or (soup-message-headers-get-one headers name)
      (soup-message-headers-get-list headers name)))

(export 'soup-message-headers-get)
