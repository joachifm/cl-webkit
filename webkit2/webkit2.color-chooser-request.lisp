;;; webkit2.color-chooser-request.lisp --- bindings for WebKitColorChooserRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitColorChooserRequest" ()
  (("rgba" "GdkRGBA")))

(defcfun "webkit_color_chooser_request_get_rgba" :void
  (request (g-object webkit-color-chooser-request))
  (rgba (g-boxed-foreign gdk:gdk-rgba)))
(export 'webkit-color-chooser-request-get-rgba)

(defcfun "webkit_color_chooser_request_set_rgba" :void
  (request (g-object webkit-color-chooser-request))
  (rgba (g-boxed-foreign gdk:gdk-rgba)))
(export 'webkit-color-chooser-request-set-rgba)

(defcfun "webkit_color_chooser_request_get_element_rectangle" :void
  (request (g-object webkit-color-chooser-request))
  (rectangle (g-boxed-foreign gdk:gdk-rectangle))) ;; XXX: GdkRectangle *
(export 'webkit-color-chooser-request-get-element-rectangle)

(defcfun "webkit_color_chooser_request_finish" :void
  (request (g-object webkit-color-chooser-request)))
(export 'webkit-color-chooser-request-finish)

(defcfun "webkit_color_chooser_request_cancel" :void
  (request (g-object webkit-color-chooser-request)))
(export 'webkit-color-chooser-request-cancel)
