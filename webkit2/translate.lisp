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

(in-package #:webkit2)

(defun translate-webkit-class-name (g-class-name)
  (cffi:translate-camelcase-name g-class-name
                                 :special-words '("DOM" "URI" "WebKit")
                                 :upper-initial-p t))

(defmethod cffi:translate-name-to-foreign (name (package (eql *package*)) &optional varp)
  (declare (ignore varp))
  (translate-underscore-separated-name name))

(defmethod cffi:translate-name-from-foreign (foreign-name (package (eql *package*)) &optional varp)
  (declare (ignore varp))
  (translate-underscore-separated-name foreign-name))
