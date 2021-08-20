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

(define-g-interface "GSeekable" g-seekable
    (:export nil
     :type-initializer "g_seekable_get_type"))
(define-g-interface "GPollableInputStream" g-pollable-input-stream
    (:export nil
     :type-initializer "g_pollable_input_stream_get_type"))

(define-g-object-class "GInputStream" g-input-stream
    (:export nil
     :type-initializer "g_input_stream_get_type")
    ())
(define-g-object-class "GMemoryInputStream" g-memory-input-stream
    (:superclass g-input-stream
     :export nil
     :interfaces ("GPollableInputStream" "GSeekable")
     :type-initializer "g_memory_input_stream_get_type")
    ())

(defcfun "g_memory_input_stream_new_from_data" (g-object g-memory-input-stream)
  (data :pointer)
  (length :long)
  (destroy :pointer))

(defcfun "g_memory_input_stream_new_from_bytes" (g-object g-memory-input-stream)
  (bytes :pointer))

(defcfun "g_bytes_new" :pointer
  (data (:pointer :uchar))
  (size :uint))

(cffi:defcallback g-notify-destroy-null :void ((data :pointer))
  (declare (ignore data)))
(cffi:defcallback g-notify-destroy-free :void ((data :pointer))
  (cffi:foreign-funcall "free" :pointer data))

(cffi:defcfun ("g_input_stream_read" %g-input-stream-read) :uint
  (stream (g:g-object webkit::g-input-stream))
  (buffer :pointer)
  (count :uint)
  (cancellable :pointer)
  (g-error :pointer))

(defun g-input-stream-read (stream buffer count)
  (glib:with-g-error (err)
    (%g-input-stream-read stream buffer count (cffi:null-pointer) err)))
