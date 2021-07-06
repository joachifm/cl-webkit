;;; tests/package.lisp -- Testing package.

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :cl-user)

(defpackage #:webkit2/tests
  (:use #:cl #:fiveam #:cffi #:gtk #:gobject))
