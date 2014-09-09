;;; util.lisp --- general purpose utilities for internal use

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

;;; note: alexandria:{format-symbol,symbolicate} are too general
;;; complicated for what we're trying to do here.
(defun string->symbol (s)
  "Convert a string to an externally usable symbol."
  (check-type s string)
  (intern (string-upcase s) *package*))
