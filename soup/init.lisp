;;; init.lisp --- initialize foreign library

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:cl-soup)

(define-foreign-library libsoup
    (:unix (:or "libsoup-2.4.so.1.7.0" ; libsoup-2.48.0
                "libsoup-2.4.so"
                )))

(use-foreign-library libsoup)
