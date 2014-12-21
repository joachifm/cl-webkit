;;; webkit2.web-page.lisp --- bindings for WebKitWebPage

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitWebPage" ()
  (("uri" "gchararray")))

(defcfun "webkit_web_page_get_dom_document" (g-object webkit-dom-document)
  (web-page (g-object webkit-web-page)))
(export 'webkit-web-page-get-dom-document)

(defcfun "webkit_web_page_get_id" :uint
  (web-page (g-object webkit-web-page)))
(export 'webkit-web-page-get-id)

(defcfun "webkit_web_page_get_main_frame" (g-object webkit-frame)
  (web-page (g-object webkit-web-page)))
(export 'webkit-web-page-get-main-frame)
