;;; webkit2.find-controller.lisp --- bindings for WebKitFindController

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(defclass webkit-find-controller (g-object)
  ((max-match-count
    :allocation :gobject-property
    :g-property-name "max-match-count"
    :g-property-type :int
    :reader find-controller-max-match-count)
   (text
    :allocation :gobject-property
    :g-property-name "text"
    :g-property-type :string
    :reader find-controller-text)
   (web-view
    :allocation :gobject-property
    :g-property-name "web-view"
    :g-property-type webkit-web-view
    :accessor find-controller-web-view
    :initarg :web-view))
  (:metaclass gobject-class)
  (:g-type-name . "WebKitFindController")
  (:g-type-initializer . "webkit_find_controller_get_type"))

(export 'webkit-find-controller)
(export 'find-controller-max-match-count)
(export 'find-controller-text)
(export 'find-controller-web-view)

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
