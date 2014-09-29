;;; package.lisp --- package definition for the test-suite

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :cl-user)

(defpackage #:cl-webkit2-tests
  (:use :cl :webkit2)
  (:nicknames #:webkit2-tests)
  (:export #:run-tests #:*expected-failure-count*))
