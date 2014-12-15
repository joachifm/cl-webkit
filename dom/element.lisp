;;; element.lisp --- binds WebKitDOMElement

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit-dom)

;;; XXX: replace opaque pointer types with proper types
(gobject:define-g-object-class "WebKitDOMElement" webkit-dom-element
  (:superclass g-object ; XXX: GObject -> WebKitDOMObject -> WebKitDOMNode -> WebKitDOMElement
               :export t
               :interfaces nil ; XXX: implements WebKitDOMEventTarget
               :type-initializer "webkit_dom_element_get_type")
  (
   ;; slot-name      accessor-name             g-object name      type     r w
   (attributes
    element-attributes "attributes" :pointer t nil)
   (child-element-count
    element-child-element-count "child-element-count" :int t nil)
   (class-list
    element-class-list "class-list" :pointer t nil)
   (class-name
    element-class-name "class-name" :string t t)
   (client-height
    element-client-height "client-height" :double t nil)
   (client-left
    element-client-left "client-left" :double t nil)
   (client-top
    element-client-top "client-top" :double t nil)
   (client-width
    element-client-width "client-width" :double t nil)
   (first-element-child
    element-first-element-child "first-element-child" :pointer t nil)
   (id
    element-id "id" :string t t)
   (last-element-child
    element-last-element-child "last-element-child" :pointer t nil)
   (next-element-sibling
    element-next-element-sibling "next-element-sibling" :pointer t nil)
   (offset-height
    element-offset-height "offset-height" :double t nil)
   (offset-left
    element-offset-left "offset-left" :double t nil)
   (offset-parent
    element-offset-parent "offset-parent" :double t nil)
   (offset-top
    element-offset-top "offset-top" :double t nil)
   (offset-width
    element-offset-width "offset-width" :double t nil)
   (previous-element-sibling
    element-previous-element-sibling "previous-element-sibling" :pointer t nil)
   (scroll-height
    element-scroll-height "scroll-height" :int t nil)
   (scroll-left
    element-scroll-left "scroll-left" :int t t)
   (scroll-top
    element-scroll-top "scroll-top" :int t t)
   (scroll-width
    element-scroll-width "scroll-width" :int t t)
   (style
    element-style "style" :pointer t nil)
   (tag-name
    element-tag-name "tag-name" :string t nil)
   (webkit-region-overset
    element-webkit-region-overset "webkit-region-overset" :string t nil)
   ))
