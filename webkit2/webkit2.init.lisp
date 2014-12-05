;;; webkit2.init.lisp --- initialize foreign library

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-foreign-library libwebkit2
    (:unix (:or "libwebkit2gtk-4.0.so"         ; webkit2gtk-2.6.4
                "libwebkit2gtk-3.0.so.25.10.9" ; webkit2gtk-2.4.6
                "libwebkit2gtk-3.0.so.25.10.8" ; webkit2gtk-2.4.5
                "libwebkit2gtk-3.0.so.25.10.7" ; webkit2gtk-2.4.4
                "libwebkit2gtk-3.0.so.25")))

(use-foreign-library libwebkit2)

(defcfun "webkit_get_major_version" :int)
(defcfun "webkit_get_minor_version" :int)

(eval-when (:load-toplevel :execute)
  (with-standard-io-syntax
    (let ((versym (intern (format nil "WEBKIT2-~a.~a"
                                  (webkit-get-major-version)
                                  (webkit-get-minor-version))
                          :keyword)))
      (pushnew versym *features*)))
  (pushnew :WEBKIT2 *features*))
