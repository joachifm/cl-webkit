;;; event.lisp --- binding to WebKitDOMEvent

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit-dom)

;;; XXX: replace opaque pointer types with proper types
(gobject:define-g-object-class "WebKitDOMEvent" webkit-dom-event
  (:superclass g-object ; XXX: GObject -> WebKitDOMObject -> WebKitDOMEvent
               :export t
               :interfaces nil
               :type-initializer "webkit_dom_event_get_type")
  (
   ;; slot-name      accessor-name             g-object name      type     r w
   (bubbles event-bubbles "bubbles" :boolean t nil)
   (cancel-bubble event-cancel-bubble "cancel-bubble" :boolean t t)
   (cancelable event-cancelable "cancelable" :boolean t nil)
   (current-target event-current-target "current-target" :pointer t nil)
   (default-prevented event-default-prevented "default-prevented" :boolean t nil)
   (event-phase event-phase "event-phase" :int t nil)
   (return-value event-return-value "return-value" :boolean t t)
   (src-element event-src-element "src-element" :pointer t nil)
   (target event-target "target" :pointer t nil)
   (time-stamp event-time-stamp "time-stamp" :int t nil)
   (type event-type "type" :string t nil)
   ))
