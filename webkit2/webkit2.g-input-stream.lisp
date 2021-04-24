;;; webkit2.g-input-stream.lisp --- bindings for GInputStream & derivatives

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Commentary

;; This should ideally be part of cl-cffi-gtk, but it's not complete
;; enough to merge to upstream, while we urgently need it for
;; WebKitURISchemeRequest bindings.

;;; Code:

(in-package #:webkit2)

(define-g-object-class "GInputStream" g-input-stream
    (:export nil
     :type-initializer "g_input_stream_get_type")
    ())
(define-g-object-class "GMemoryInputStream" g-memory-input-stream
    (:superclass g-input-stream
     :export nil
     :type-initializer "g_memory_input_stream_get_type")
    ())

(defcfun "g_memory_input_stream_new_from_data" (g-object g-memory-input-stream)
  (data :pointer)
  (length :long)
  (destroy :pointer))

(cffi:defcallback g-notify-destroy-null :void ((data :pointer))
  (declare (ignore data)))
(cffi:defcallback g-notify-destroy-free :void ((data :pointer))
  (cffi:foreign-funcall "free" :pointer data))
