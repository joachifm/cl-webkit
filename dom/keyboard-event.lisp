;;; keyboard-event.lisp --- binds WebKitDOMKeyboardEvent

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit-dom)

(gobject:define-g-object-class "WebKitDOMKeyboardEvent" webkit-dom-keyboard-event
  (:superclass g-object ; XXX: GObject -> WebKitDOMObject -> WebKitDOMEvent -> WebKitDOMUIEvent
               :export t
               :interfaces nil
               :type-initializer "webkit_dom_keyboard_event_get_type")
  (
   ;; slot-name accessor-name g-object-name type r w
   (alt-graph-key keyboard-event-alt-graph-key "alt-graph-key" :boolean t nil)
   (alt-key keyboard-event-alt-key "alt-key" :boolean t nil)
   (ctrl-key keyboard-event-ctrl-key "ctrl-key" :boolean t nil)
   (key-identifier keyboard-event-key-identifier "key-identifier" :string t nil)
   (key-location keyboard-event-key-location "key-location" :int t nil)
   (meta-key keyboard-event-meta-key "meta-key" :boolean t nil)
   (shift-key keyboard-event-shift-key "shift-key" :boolean t nil)))
