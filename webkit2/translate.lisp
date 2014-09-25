;;; translate.lisp -- cffi translations

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Commentary:
;;
;; This module defines name and type translations used by all bindings.

;;; Code:

(in-package :webkit2)

(defmethod cffi:translate-name-to-foreign (name (package (eql *package*)) &optional varp)
  (declare (ignore varp))
  (concatenate 'string "webkit_"
               (translate-underscore-separated-name name)))

(defmethod cffi:translate-name-from-foreign (foreign-name (package (eql *package*)) &optional varp)
  (declare (ignore varp))
  (let ((basename (subseq foreign-name 7))) ; drop prefix "webkit_"
    (translate-underscore-separated-name basename)))
