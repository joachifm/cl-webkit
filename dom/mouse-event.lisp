;;; mouse-event.lisp --- binds WebKitDOMMouseEvent

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit-dom)

(gobject:define-g-object-class "WebKitDOMMouseEvent" webkit-dom-mouse-event
  (:superclass g-object ; XXX: GObject -> WebKitDOMObject -> WebKitDOMEvent -> WebKitDOMUIEvent
               :export t
               :interfaces nil
               :type-initializer "webkit_dom_mouse_event_get_type")
  (
   ;; slot-name accessor-name g-object-name type r w
   (alt-key mouse-event-alt-key "alt-key" :boolean t nil)
   (button mouse-event-button "button" :int t nil)
   (client-x mouse-event-client-x "client-x" :int t nil)
   (client-y mouse-event-client-y "client-y" :int t nil)
   (ctrl-key mouse-event-ctrl-key "ctrl-key" :boolean t nil)
   (from-element mouse-event-from-element "from-element" :pointer t nil) ; XXX: WebKitDOMNode *
   (meta-key mouse-event-meta-key "meta-key" :boolean t nil)
   (movement-x mouse-event-movement-x "movement-x" :int t nil)
   (movement-y mouse-event-movement-y "movement-y" :int t nil)
   (offset-x mouse-event-offset-x "offset-x" :int t nil)
   (offset-y mouse-event-offset-y "offset-y" :int t nil)
   (related-target mouse-event-related-target "related-target" :pointer t nil) ; XXX: WebKitDOMEventTarget *
   (screen-x mouse-event-screen-x "screen-x" :int t nil)
   (screen-y mouse-event-screen-y "screen-y" :int t nil)
   (shift-key mouse-event-shift-key "shift-key" :boolean t nil)
   (to-element mouse-event-to-element "to-element" :pointer t nil) ; XXX: WebKitDOMNode *
   (x mouse-event-x "x" :int t nil)
   (y mouse-event-y "y" :int t nil)))
