;;; simple-browser.lisp --- webkit browser demo

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

(require :asdf)
(asdf:load-systems :cl-cffi-gtk :cl-webkit2)

(defun simple-browser-main ()
  "A single-window browser with no keyboard or mouse input.
Loads and renders a single web page."
  (gtk:within-main-loop
    (let* ((win (make-instance 'gtk:gtk-window))
           (manager (make-instance 'webkit:webkit-website-data-manager
                                   :base-data-directory "testing-data-manager"))
           (context (make-instance 'webkit:webkit-web-context
                                   :website-data-manager manager))
           (view (make-instance 'webkit2:webkit-web-view
                                :web-context context)))
      (gobject:g-signal-connect win "destroy"
                                #'(lambda (widget)
                                    (declare (ignore widget))
                                    (gtk:leave-gtk-main)))
      (gtk:gtk-container-add win view)
      (webkit2:webkit-web-view-load-uri view "http://www.example.com")
      (gtk:gtk-widget-show-all win))))

(defun private-browser-main ()
  "An ephemeral (a.k.a private) mode version of `simple-browser-main'."
  (gtk:within-main-loop
   (let* ((win (make-instance 'gtk:gtk-window))
          (context (webkit:webkit-web-context-new-ephemeral))
          (view (make-instance 'webkit2:webkit-web-view :web-context context)))
     (gobject:g-signal-connect win "destroy"
                               #'(lambda (widget)
                                   (declare (ignore widget))
                                   (gtk:leave-gtk-main)))
     (gtk:gtk-container-add win view)
     (webkit2:webkit-web-view-load-uri view "http://www.example.com")
     (gtk:gtk-widget-show-all win))))

(defun do-simple-browser-main (&key (private nil))
  (if private
      (private-browser-main)
      (simple-browser-main))
  (gtk:join-gtk-main))
