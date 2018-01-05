;;; jscore.asd --- ASDF system definition

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

(defsystem :jscore
  :description "A FFI binding to JSCore"
  :licence "MIT"
  :version "0.0"
  :serial t
  :components ((:file "jscore.package")
               (:file "jscore.init")
               (:file "jscore.js-value-ref"))
  :depends-on (:cffi :cl-cffi-gtk
               :cl-soup :cl-webkit-dom))
