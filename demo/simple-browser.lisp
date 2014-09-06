;;; simple-browser.lisp --- webkit browser demo

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Commentary:
;;
;; The simplest example: a WebKit frame which loads and renders a single website.

;;; Code:

(in-package :cl-user)

(mapcar #'asdf:load-system '(:cl-cffi-gtk :cl-webkit2))

(defun simple-browser-main ()
  "A single-window browser with no keyboard or mouse input.
Loads and renders a single web page."
  (gtk:within-main-loop
    (let ((win (make-instance 'gtk:gtk-window))
          (view (make-instance 'webkit2:webkit-web-view)))
      (gobject:g-signal-connect win "destroy"
                                #'(lambda (widget)
                                    (declare (ignore widget))
                                    (gtk:leave-gtk-main)))
      (gtk:gtk-container-add win view)
      (webkit2:webkit-web-view-load-uri view "http://www.example.com")
      (gtk:gtk-widget-show-all win))))
