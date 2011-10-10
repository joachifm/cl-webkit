(in-package :cl-user)

(defpackage :cl-webkit-demo-asd
  (:use :cl :asdf))

(in-package :cl-webkit-demo-asd)

(defsystem :cl-webkit-demo
  :description "cl-webkit usage examples"
  :author "Joachim Fasting <joachim.fasting@gmail.com>"
  :licence "BSD"
  :version "0.0"
  :serial t
  :components ((:file "demo"))
  :depends-on (:cl-gtk2-gtk :cl-webkit))
