(in-package :cl-user)

(defpackage :webkit
  (:use :cl :cffi)
  (:export
   #:webkit-web-view-new
   #:webkit-web-view-load-uri
   #:webkit-web-view-get-title
   #:webkit-web-view-get-editable
   #:webkit-web-view-set-editable))
