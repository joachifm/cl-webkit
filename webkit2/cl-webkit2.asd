;;; cl-webkit2.asd --- ASDF system definition

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :cl-user)

(defpackage :cl-webkit2-asd
  (:use :cl :asdf))

(in-package :cl-webkit2-asd)

(defsystem :cl-webkit2
  :description "An FFI binding to WebKit2GTK+"
  :author "Joachim Fasting <joachifm@fastmail.fm>"
  :licence "MIT"
  :version "0.0"
  :serial t
  :components ((:file "webkit2.package")
               (:file "webkit2.init")
               (:file "webkit2.find-controller")
               (:file "webkit2.web-view")
               (:file "webkit2.cookie-manager")
               (:file "webkit2.download")
               (:file "webkit2.web-context" :depends-on ("webkit2.download")))
  :depends-on (:cffi))
