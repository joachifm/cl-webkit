;;; webkit2.context-menu-item.lisp --- bindings for WebKitContextMenuItem

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitContextMenuItem" () ()) ; XXX: GInitiallyUnowned

(define-g-enum "WebKitContextMenuAction" webkit-context-menu-action ()
  :webkit-context-menu-action-no-action
  :webkit-context-menu-action-open-link
  :webkit-context-menu-action-open-link-in-new-window
  :webkit-context-menu-action-download-link-to-disk
  :webkit-context-menu-action-copy-link-to-clipboard
  :webkit-context-menu-action-open-image-in-new-window
  :webkit-context-menu-action-download-image-to-disk
  :webkit-context-menu-action-copy-image-to-clipboard
  :webkit-context-menu-action-copy-image-url-to-clipboard
  :webkit-context-menu-action-open-frame-in-new-window
  :webkit-context-menu-action-go-back
  :webkit-context-menu-action-go-forward
  :webkit-context-menu-action-stop
  :webkit-context-menu-action-reload
  :webkit-context-menu-action-copy
  :webkit-context-menu-action-cut
  :webkit-context-menu-action-paste
  :webkit-context-menu-action-delete
  :webkit-context-menu-action-select-all
  :webkit-context-menu-action-input-methods
  :webkit-context-menu-action-unicode
  :webkit-context-menu-action-spelling-guess
  :webkit-context-menu-action-no-guesses-found
  :webkit-context-menu-action-ignore-spelling
  :webkit-context-menu-action-learn-spelling
  :webkit-context-menu-action-ignore-grammar
  :webkit-context-menu-action-font-menu
  :webkit-context-menu-action-bold
  :webkit-context-menu-action-italic
  :webkit-context-menu-action-underline
  :webkit-context-menu-action-outline
  :webkit-context-menu-action-inspect-element
  :webkit-context-menu-action-open-video-in-new-window
  :webkit-context-menu-action-open-audio-in-new-window
  :webkit-context-menu-action-copy-video-link-to-clipboard
  :webkit-context-menu-action-copy-audio-link-to-clipboard
  :webkit-context-menu-action-toggle-media-controls
  :webkit-context-menu-action-toggle-media-loop
  :webkit-context-menu-action-enter-video-fullscreen
  :webkit-context-menu-action-media-pause
  :webkit-context-menu-action-media-mute
  :webkit-context-menu-action-download-video-to-disk
  :webkit-context-menu-action-download-audio-to-disk
  :webkit-context-menu-action-action-custom)

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
  
(defcfun "webkit_context_menu_item_new_from_stock_action" (g-object webkit-context-menu-item)
  (action (g-object webkit-context-menu-action)))
(export 'webkit-context-menu-item-new-from-stock-action)

(defcfun "webkit_context_menu_item_new_from_stock_action_with_label" (g-object webkit-context-menu-item)
  (action (g-object webkit-context-menu-action))
  (label :string))
(export 'webkit-context-menu-item-new-from-stock-action-with-label)

(defcfun "webkit_context_menu_item_get_stock_action" (g-object webkit-context-menu-action)
  (action (g-object webkit-context-menu-action)))
(export 'webkit-context-menu-item-get-stock-action)
