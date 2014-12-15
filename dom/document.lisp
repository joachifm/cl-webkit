;;; document.lisp --- binds WebKitDOMDocument

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit-dom)

;;; XXX: replace opaque pointer types with proper types
(gobject:define-g-object-class "WebKitDOMDocument" webkit-dom-document
  (:superclass g-object ; XXX: GObject -> WebKitDOMObject -> WebKitDOMNode -> WebKitDOMDocument
               :export t
               :interfaces nil ; XXX: implements WebKitDOMEventTarget
               :type-initializer "webkit_dom_document_get_type")
  (
  ;; slot-name      accessor-name             g-object name      type     r w
  (active-element   document-active-element  "active-element"   :pointer t nil)
  (anchors          document-anchors         "anchors"          :pointer t nil)
  (applets          document-applets         "applets"          :pointer t nil)
  (body             document-body            "body"             :pointer t nil)
  (character-set    document-character-set   "character-set"    :string  t nil)
  (charset          document-charset         "charset"          :string  t t  )
  (compat-mode      document-compat-mode     "compat-mode"      :string  t nil)
  (cookie           document-cookie          "cookie"           :string  t t  )
  (current-script   document-current-script  "current-script"   :pointer t nil)
  (default-charset  document-default-charset "default-charset"  :string  t nil)
  (default-view     document-default-view    "default-view"     :pointer t nil)
  (doctype          document-doctype         "doctype"          :pointer t nil)
  (document-element document-element         "document-element" :pointer t nil)
  (document-uri     document-uri             "document-uri"     :string  t t  )
  (domain           document-domain          "domain"           :string  t nil)
  (forms            document-forms           "forms"            :pointer t nil)
  (head             document-head            "head"             :pointer t nil)
  (hidden           document-hidden          "hidden"           :boolean t nil)
  (images           document-images          "images"           :pointer t nil)
  (implementation   document-implementation  "implementation"   :pointer t nil)
  (input-encoding   document-input-encoding  "input-encoding"   :string  t nil)
  (last-modified    document-last-modified   "last-modified"    :string  t nil)
  (links            document-links           "links"            :pointer t nil)
  (origin           document-origin          "origin"           :string  t nil)
  (pointer-lock-element
   document-pointer-lock-element "pointer-lock-element" :pointer t nil)
  (preferred-stylesheet-set
   document-preferred-stylesheet-set "preferred-stylesheet-set" :string  t nil)
  (ready-state      document-ready-state     "ready-state"      :string  t nil)
  (referrer         document-referrer        "referrer"         :string  t nil)
  (security-policy  document-security-policy "security-policy"  :pointer t nil)
  (selected-stylesheet-set
   document-selected-stylesheet-set "selected-stylesheet-set" :string t t)
  (style-sheets     document-style-sheets     "style-sheets"     :pointer t nil)
  (title            document-title            "title"            :string  t t  )
  (url              document-url              "url"              :string  t nil)
  (visibility-state document-visibility-state "visibility-state" :string t nil)
  (webkit-current-full-screen-element
   document-webkit-current-full-screen-element
   "webkit-current-full-screen-element" :pointer t nil)
  (webkit-full-screen-keyboard-input-allowed
   document-webkit-full-screen-keyboard-input-allowed
   "webkit-full-screen-keyboard-input-allowed"
   :boolean t nil)
  (webkit-fullscreen-element
   document-webkit-fullscreen-element
   "webkit-fullscreen-element"
   :pointer t nil)
  (webkit-fullscreen-enabled
   document-webkit-fullscreen-enabled
   "webkit-fullscreen-enabled"
   :boolean t nil)
  (webkit-is-full-screen
   document-webkit-is-full-screen
   "webkit-is-full-screen"
   :boolean t nil)
  (xml-encoding   document-xml-encoding   "xml-encoding"   :string  t nil)
  (xml-standalone document-xml-standalone "xml-standalone" :boolean t t  )
  (xml-version    document-xml-version    "xml-version"    :string  t t  )))
