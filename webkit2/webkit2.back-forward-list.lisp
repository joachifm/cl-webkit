;;; webkit2.back-forward-list.lisp --- bindings for WebKitBackForwardList

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitBackForwardList" () ())

(defcfun "webkit_back_forward_list_get_length" :uint
  (back-forward-list (g-object webkit-back-forward-list)))
(export 'webkit-back-forward-list-get-length)

(defcfun "webkit_back_forward_list_get_current_item" (g-object webkit-back-forward-list-item)
  (back-forward-list (g-object webkit-back-forward-list)))
(export 'webkit-back-forward-list-get-current-item)

(defcfun "webkit_back_forward_list_get_back_item" (g-object webkit-back-forward-list-item)
  (back-forward-list (g-object webkit-back-forward-list)))
(export 'webkit-back-forward-list-get-back-item)

(defcfun "webkit_back_forward_list_get_forward_item" (g-object webkit-back-forward-list-item)
  (back-forward-list (g-object webkit-back-forward-list)))
(export 'webkit-back-forward-list-get-forward-item)

(defcfun "webkit_back_forward_list_get_nth_item" (g-object webkit-back-forward-list-item)
  (back-forward-list (g-object webkit-back-forward-list))
  (index :int))
(export 'webkit-back-forward-list-get-nth-item)
