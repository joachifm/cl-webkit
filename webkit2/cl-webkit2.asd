;;; cl-webkit2.asd --- ASDF system definition

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :asdf-user)

(defsystem :cl-webkit2
  :description "An FFI binding to WebKit2GTK+"
  :author "Joachim Fasting <joachifm@fastmail.fm>"
  :licence "MIT"
  :version "0.0"
  :serial t
  :components ((:file "webkit2.package")
               (:file "webkit2.init")
               (:file "util")
               (:file "macros" :depends-on ("util"))
               (:file "webkit2.find-controller")
               (:file "webkit2.cookie-manager")
               (:file "webkit2.uri-response")
               (:file "webkit2.download" :depends-on ("webkit2.uri-response"))
               (:file "webkit2.settings")
               (:file "webkit2.uri-request")
               (:file "webkit2.web-resource" :depends-on ("webkit2.uri-response"))
               (:file "webkit2.web-context" :depends-on ("webkit2.download"))
               (:file "webkit2.web-view" :depends-on ("macros"
                                                      "webkit2.settings"
                                                      "webkit2.uri-request"
                                                      "webkit2.web-context"
                                                      "webkit2.web-resource")))
  :depends-on (:cffi :cl-cffi-gtk))
