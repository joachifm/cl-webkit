;;; jscore.jsc-class.lisp --- bindings for JSCClass

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "JSCClass"
    (:type-initializer "jsc_class_get_type")
    (("context" "JSCContext")
     ("name" "gchararray")
     ("parent" "JSCClass")))
