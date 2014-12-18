;;; util.lisp --- general purpose utilities for internal use

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(defun foo->symbol (&rest lst)
  "Convert a list of things (coercible to string) into a symbol."
  (let ((s (apply #'concatenate 'string (mapcar #'string lst))))
    (intern (string-upcase s)
            (load-time-value (find-package ':cl-webkit2)))))
