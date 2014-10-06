;;; webkit2.context-menu-item.lisp --- bindings for WebKitContextMenuItem

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitContextMenuItem" () ()) ; XXX: GInitiallyUnowned

(defcfun "webkit_context_menu_item_new" (g-object webkit-context-menu-item)
  (action (g-object gtk:gtk-action)))
(export 'webkit-context-menu-item-new)

(defcfun "webkit_context_menu_item_new_with_submenu" (g-object webkit-context-menu-item)
  (label :string)
  (submenu (g-object webkit-context-menu)))
(export 'webkit-context-menu-item-new-with-submenu)

(defcfun "webkit_context_menu_item_new_separator" (g-object webkit-context-menu-item))
(export 'webkit-context-menu-item-new-separator)

(defcfun "webkit_context_menu_item_get_action" (g-object gtk:gtk-action)
  (item (g-object webkit-context-menu-item)))
(export 'webkit-context-menu-item-get-action)

(defcfun "webkit_context_menu_item_is_separator" :boolean
  (item (g-object webkit-context-menu-item)))
(export 'webkit-context-menu-item-is-separator)
  
