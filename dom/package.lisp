;;; package.lisp --- package definition

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :cl-user)

(defpackage #:cl-webkit-dom
  (:nicknames :webkit-dom :dom)
  (:use :cl :cffi))
