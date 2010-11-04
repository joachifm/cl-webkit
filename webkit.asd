;;;; ^L
;;;; WEBKIT system definition

(declaim (optimize (debug 3) (compilation-speed 3)))

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cffi-grovel))

(defpackage :webkit-asd
  (:use :cl :asdf))

(in-package :webkit-asd)

(defsystem :webkit
  :description "A simple FFI for webkit"
  :author "Joachim Fasting <joachim.fasting@gmail.com>"
  :licence "BSD"
  :version "0.0"
  :serial t
  :components ((:file "packages")
               (cffi-grovel:grovel-file "webkit-grovelling")
               (:file "webkit"))
  :depends-on (:cffi))
