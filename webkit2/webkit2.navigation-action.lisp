;;; webkit2.navigation-action.lisp --- bindings for WebKitNavigationAction

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(defctype webkit-navigation-action :pointer) ; XXX: GBoxed

(defcfun "webkit_navigation_action_copy" webkit-navigation-action
  (navigation webkit-navigation-action))
(export 'webkit-navigation-action-copy)

(defcfun "webkit_navigation_action_free" :void
  (navigation webkit-navigation-action))
(export 'webkit-navigation-action-free)

(defcfun "webkit_navigation_action_get_navigation_type" webkit-navigation-type
  (navigation webkit-navigation-action))
(export 'webkit-navigation-action-get-navigation-type)

(defcfun "webkit_navigation_action_get_mouse_button" :int
  (navigation webkit-navigation-action))
(export 'wekbkit-navigation-action-get-mouse-button)

(defcfun "webkit_navigation_action_get_modifiers" :int ;; XXX GdkModifierType bitmask
  (navigation webkit-navigation-action))
(export 'webkit-navigation-action-get-modifiers)

(defcfun "webkit_navigation_action_get_request" (g-object webkit-uri-request)
  (navigation webkit-navigation-action))
(export 'webkit-navigation-action-get-request)

(defcfun "webkit_navigation_action_is_user_gesture" :boolean
  (navigation webkit-navigation-action))
(export 'webkit-navigation-action-is-user-gesture)
