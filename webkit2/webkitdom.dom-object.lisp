;;; webkitdom.dom-object.lisp --- bindings for WebKitDOMObject

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitDOMObject" () ())

(define-webkit-class "WebKitDOMNode"
    (:superclass webkit-dom-object)
    (("base-uri" "gchararray")
     ("child-nodes" "WebKitDOMNodeList")
     ("first-child" "WebKitDOMNode")
     ("last-child" "WebKitDOMNode")
     ("next-sibling" "WebKitDOMNode")
     ("node-name" "gchararray")
     ("node-type" "guint")
     ("node-value" "gchararray")
     ("owner-document" "WebKitDOMDocument")
     ("parent-element" "WebKitDOMElement")
     ("parent-node" "WebKitDOMNode")
     ("previous-sibling" "WebKitDOMNode")
     ("text-content" "gchararray")))

(define-webkit-class "WebKitDOMDocument"
    (:superclass webkit-dom-node)
    (("active-element" "WebKitDOMElement")
     ("anchors" "WebKitDOMHTMLCollection")
     ("applets" "WebKitDOMHTMLCollection")
     ("body" "WebKitDOMHTMLElement")
     ("character-set" "gchararray")
     ("charset" "gchararray")
     ("child-element-count" "gulong")
     ("children" "WebKitDOMHTMLCollection")
     ("compat-mode" "gchararray")
     ("content-type" "gchararray")
     ("cookie" "gchararray")
     ("current-script" "WebKitDOMHTMLScriptElement")
     ("default-view" "WebKitDOMDOMWindow")
     ("design-mode" "gchararray")
     ("dir" "gchararray")
     ("doctype" "WebKitDOMDocumentType")
     ("document-element" "WebKitDOMElement")
     ("document-uri" "gchararray")
     ("domain" "gchararray")
     ("embeds" "WebKitDOMHTMLCollection")
     ("first-element-child" "WebKitDOMElement")
     ("forms" "WebKitDOMHTMLCollection")
     ("head" "WebKitDOMHTMLHeadElement")
     ("hidden" "gboolean")
     ("images" "WebKitDOMHTMLCollection")
     ("implementation" "WebKitDOMDOMImplementation")
     ("input-encoding" "char")
     ("last-element-child" "WebKitDOMElement")
     ("last-modified" "gchararray")
     ("links" "WebKitDOMHTMLCollection")
     ("origin" "gchararray")
     ("plugins" "WebKitDOMHTMLCollection")
     ("pointer-lock-element" "WebKitDOMElement")
     ("preferred-stylesheet-set" "gchararray")
     ("ready-state" "gchararray")
     ("referrer" "gchararray")
     ("scripts" "WebKitDOMHTMLCollection")
     ("scrolling-element" "WebKitDOMElement")
     ("selected-stylesheet-set" "gchararray")
     ("style-sheets" "WebKitDOMStyleSheetList")
     ("title" "gchararray")
     ("url" "gchararray")
     ("visibility-state" "gchararray")
     ("webkit-current-full-screen-element" "WebKitDOMElement")
     ("webkit-full-screen-keyboard-input-allowed" "gboolean")
     ("webkit-fullscreen-element" "WebKitDOMElement")
     ("webkit-fullscreen-enabled" "gboolean")
     ("webkit-is-full-screen" "gboolean")
     ("xml-encoding" "ghararray")
     ("xml-standalone" "gboolean")
     ("xml-version" "gchararray")))

