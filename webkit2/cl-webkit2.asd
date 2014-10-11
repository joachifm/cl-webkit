;;; cl-webkit2.asd --- ASDF system definition

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

#-(or clisp sbcl)
(warn "unsupported implementation, satisfaction uncertain!")

#-asdf3
(defpackage #:asdf-user (:use :cl :asdf))

(in-package :asdf-user)

(defsystem :cl-webkit2
  :description "An FFI binding to WebKit2GTK+"
  :author "Joachim Fasting <joachifm@fastmail.fm>"
  :licence "MIT"
  :version "0.0"
  :serial t
  :components ((:file "webkit2.package")
               (:file "util")
               (:file "macros")
               (:file "translate")
               (:file "webkit2.init")
               (:file "webkit2.back-forward-list-item")
               (:file "webkit2.back-forward-list")
               (:file "webkit2.cookie-manager")
               (:file "webkit2.context-menu-item")
               (:file "webkit2.context-menu")
               (:file "webkit2.download")
               (:file "webkit2.favicon-database")
               (:file "webkit2.file-chooser-request")
               (:file "webkit2.find-controller")
               (:file "webkit2.form-submission-request")
               (:file "webkit2.hit-test-result")
               (:file "webkit2.permission-request")
               (:file "webkit2.plugin")
               (:file "webkit2.print-operation")
               (:file "webkit2.script-world")
               (:file "webkit2.frame")
               (:file "webkit2.security-manager")
               (:file "webkit2.settings")
               (:file "webkit2.uri-request")
               (:file "webkit2.uri-response")
               (:file "webkit2.policy-decision")
               (:file "webkit2.navigation-policy-decision")
               (:file "webkit2.response-policy-decision")
               (:file "webkit2.web-context")
               (:file "webkit2.web-resource")
               (:file "webkit2.web-view-group")
               (:file "webkit2.web-view")
               (:file "webkit2.web-inspector")
               (:file "webkit2.window-properties"))
  :depends-on (:cffi :cl-cffi-gtk))
