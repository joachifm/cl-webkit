;;; webkit2.package.lisp --- package definition

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :cl-user)

(defpackage :cl-webkit2
  (:nicknames :webkit2 :webkit)
  (:use :cl :cffi)
  (:export
   #:webkit-cache-model-document-viewer
   #:webkit-cache-model-web-browser
   #:webkit-cache-model-document-browser
   #:webkit-tls-errors-policy-ignore
   #:webkit-tls-errors-policy-fail
   #:webkit-process-model-shared-secondary-process
   #:webkit-process-model-multiple-secondary-processes
   #:webkit-cookie-persistent-storage-text
   #:webkit-cookie-persistent-storage-sqlite
   #:webkit-cookie-policy-accept-always
   #:webkit-cookie-policy-accept-never
   #:webkit-cookie-policy-accept-no-third-party))
