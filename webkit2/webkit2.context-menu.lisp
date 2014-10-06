;;; webkit2.context-menu.lisp --- bindings for WebKitContextMenu

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitContextMenu" () ())

(defcfun "webkit_context_menu_new" (g-object webkit-context-menu))
(export 'webkit-context-menu-new)

(defcfun "webkit_context_menu_new_with_items" (g-object webkit-context-menu)
  (items (glib:g-list webkit-context-menu-item)))
(export 'webkit-context-menu-new-with-items)

(defcfun "webkit_context_menu_prepend" :void
  (menu (g-object webkit-context-menu))
  (item (g-object webkit-context-menu-item)))
(export 'webkit-context-menu-prepend)

(defcfun "webkit_context_menu_append" :void
  (menu (g-object webkit-context-menu))
  (item (g-object webkit-context-menu-item)))
(export 'webkit-context-menu-append)

(defcfun "webkit_context_menu_insert" :void
  (menu (g-object webkit-context-menu))
  (item (g-object webkit-context-menu-item))
  (position :int))
(export 'webkit-context-menu-insert)

(defcfun "webkit_context_menu_move_item" :void
  (menu (g-object webkit-context-menu))
  (item (g-object webkit-context-menu-item))
  (position :int))
(export 'webkit-context-menu-move-item)

(defcfun "webkit_context_menu_get_items" (glib:g-list webkit-context-menu-item)
  (menu (g-object webkit-context-menu)))
(export 'webkit-context-menu-get-items)

(defcfun "webkit_context_menu_get_n_items" :uint
  (menu (g-object webkit-context-menu)))
(export 'webkit-context-menu-get-n-items)

(defcfun "webkit_context_menu_first" (g-object webkit-context-menu-item)
  (menu (g-object webkit-context-menu)))
(export 'webkit-context-menu-first)

(defcfun "webkit_context_menu_last" (g-object webkit-context-menu-item)
  (menu (g-object webkit-context-menu)))
(export 'webkit-context-menu-last)

(defcfun "webkit_context_menu_get_item_at_position" (g-object webkit-context-menu-item)
  (menu (g-object webkit-context-menu))
  (position :uint))
(export 'webkit-context-menu-get-item-at-position)

(defcfun "webkit_context_menu_remove" :void
  (menu (g-object webkit-context-menu))
  (item (g-object webkit-context-menu-item)))
(export 'webkit-context-menu-remove)

(defcfun "webkit_context_menu_remove_all" :void
  (menu (g-object webkit-context-menu)))
(export 'webkit-context-menu-remove-all)
