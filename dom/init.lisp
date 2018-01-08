;;; init.lisp --- initialize foreign library

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit-dom)

(define-foreign-library libwebkit2
    (:unix (:or "libwebkit2gtk-3.0.so")))

(use-foreign-library libwebkit2)
