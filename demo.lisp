;;; demo.lisp - usage examples
;; Copyright (C) 2011, Joachim Fasting
;;
;;   This file is part of cl-webkit
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistribution of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistribution in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; Code:

(defpackage :cl-webkit-demo
  (:use :cl)
  (:nicknames "webkit-demo")
  (:export #:simple-browser))

(in-package :cl-webkit-demo)

(defun simple-browser ()
  "A single-window browser with no keyboard or mouse input.
Loads and renders a single web page."
  (gtk:within-main-loop
    (let ((win (make-instance 'gtk:gtk-window))
          (scrolled (make-instance 'gtk:scrolled-window))
          (view (webkit.foreign:webkit-web-view-new)))
      (gtk:container-add scrolled view)
      (gtk:container-add win scrolled)
      (webkit.foreign:webkit-web-view-load-uri view "http://www.example.com")
      (gtk:widget-show win))))
