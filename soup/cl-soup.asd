;;; cl-soup.asd --- ASDF system definition

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

(defsystem :cl-soup
  :description "An FFI binding to libsoup"
  :author "Joachim Fasting <joachifm@fastmail.fm>"
  :licence "MIT"
  :version "0.0"
  :serial t
  :components ((:file "package")
               (:file "init")
               (:file "message-headers"))
  :depends-on (:cffi))
