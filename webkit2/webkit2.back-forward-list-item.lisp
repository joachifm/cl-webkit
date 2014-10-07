;;; webkit2.back-forward-list-item.lisp --- bindings for WebKitBackForwardListItem

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitBackForwardListItem" () ()) ; XXX: GInitiallyUnowned

(defcfun "webkit_back_forward_list_item_get_title" :string
  (list-item (g-object webkit-back-forward-list-item)))
(export 'webkit-back-forward-list-item-get-title)

(defcfun "webkit_back_forward_list_item_get_uri" :string
  (list-item (g-object webkit-back-forward-list-item)))
(export 'webkit-back-forward-list-item-get-uri)

(defcfun "webkit_back_forward_list_item_get_original_uri" :string
  (list-item (g-object webkit-back-forward-list-item)))
(export 'webkit-back-forward-list-item-get-original-uri)
