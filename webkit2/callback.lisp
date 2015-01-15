;;; callback.lisp --- C-side callbacks

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(defctype g-async-result :pointer)
(defctype g-async-ready-callback :pointer)

(defmacro define-g-async-ready-callback (name &body body)
  `(cffi:defcallback ,name :void ((source-object :pointer)
                                  (result g-async-result)
                                  (user-data :pointer))
                     `@body))

(defmacro with-g-async-ready-callback ((var &body callback-body) &body body)
  (let ((g (gensym "CALLBACK")))
    `(progn
       (define-g-async-ready-callback ,g
           ,@callback-body)
       (let ((,var (callback ,g)))
         ,@body)
       (fmakeunbound ,g))))
