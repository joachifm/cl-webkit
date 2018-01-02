;;; cl-webkit-dom.asd --- ASDF system definition

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

#-(or clisp sbcl ccl)
(warn "unsupported implementation, satisfaction uncertain!")

#-asdf3
(defpackage #:asdf-user (:use :cl :asdf))

(in-package :asdf-user)

(defsystem :cl-webkit-dom
  :description "Interface to the WebKit2 Document Object Model"
  :author "Joachim Fasting <joachifm@fastmail.fm>"
  :licence "MIT"
  :version "0.0"
  :serial t
  :components ((:file "package")
               (:file "init")
               (:file "element")
               (:file "document")
               (:file "event")
               (:file "keyboard-event")
               (:file "mouse-event"))
  :depends-on (:cffi :cl-cffi-gtk :cl-cffi-gtk-gobject))
