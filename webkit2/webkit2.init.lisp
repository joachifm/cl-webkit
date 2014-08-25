;;; webkit2.init.lisp --- initialize foreign library

;; Copyright (c) 2014 Joachim Fasting
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-foreign-library libwebkit
  (:unix (:or "libwebkit2gtk-3.0.so")))

(use-foreign-library libwebkit)
