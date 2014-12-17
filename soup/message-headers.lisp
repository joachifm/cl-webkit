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
(export 'soup-message-headers-get-one)

(defcfun "soup_message_headers_get_list" :string
  (headers soup-message-headers)
  (name :string))
(export 'soup-message-headers-get-list)
