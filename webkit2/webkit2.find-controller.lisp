;;; webkit2.find-controller.lisp --- bindings for WebKitFindController

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitFindController" ()
  (("max-match-count" "guint")
   ("text" "gchararray")
   ("web-view" "WebKitWebView" t t)))

(defcfun "webkit_find_controller_search" :void
  (find-controller (g-object webkit-find-controller))
  (search-text :string)
  (find-options :uint)
  (max-match-count :uint))
(export 'webkit-find-controller-search)

(defcfun "webkit_find_controller_search_next" :void
  (find-controller (g-object webkit-find-controller)))
(export 'webkit-find-controller-search-next)

(defcfun "webkit_find_controller_search_previous" :void
  (find-controller (g-object webkit-find-controller)))
(export 'webkit-find-controller-search-previous)

(defcfun "webkit_find_controller_search_finish" :void
  (find-controller (g-object webkit-find-controller)))
(export 'webkit-find-controller-search-finish)
