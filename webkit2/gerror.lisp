;;; gerror.lisp --- bindings for g boxed gerror structs

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-g-boxed-cstruct g-error "GError"
  (:domain :uint32) ;; :domain is a gquark
  (:code :int)
  (:message (:string :free-from-foreign nil)))
