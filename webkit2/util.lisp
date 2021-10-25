;;; util.lisp --- general purpose utilities for internal use

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(defun foo->symbol (&rest lst)
  "Convert a list of things (coercible to string) into a symbol."
  (let ((s (apply #'concatenate 'string (mapcar #'string lst))))
    (intern (string-upcase s)
            (load-time-value (find-package ':cl-webkit2)))))

(defun g-variant-get-maybe-string (variant)
  "Get a string out of the VARIANT.
If it's NULL, return nil."
  (cond
    ((null variant) nil)
    ((cffi:null-pointer-p variant) nil)
    ((glib:g-variant-type-is-maybe (glib:g-variant-get-type variant))
     (let ((maybe (cffi:foreign-funcall "g_variant_get_maybe" (:pointer (:struct glib:g-variant))
                                        variant (:pointer (:struct glib:g-variant)))))
       (unless (or (null maybe) (cffi:null-pointer-p maybe))
         (glib:g-variant-get-string maybe))))
    (t (glib:g-variant-get-string variant))))
