;;; webkit2.uri-scheme-request.lisp --- bindings for WebKitURISchemeRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitURISchemeRequest" () ())

(define-g-object-class "GInputStream" g-input-stream
    (:export nil
     :type-initializer "g_input_stream_get_type")
    ())
(define-g-object-class "GMemoryInputStream" g-memory-input-stream
    (:superclass g-input-stream
     :export nil
     :type-initializer "g_memory_input_stream_get_type")
    ())

(defcfun "webkit_uri_scheme_request_get_scheme" :string
  (request (g-object webkit-uri-scheme-request)))
(export 'webkit-uri-scheme-request-get-scheme)

(defcfun "webkit_uri_scheme_request_get_uri" :string
  (request (g-object webkit-uri-scheme-request)))
(export 'webkit-uri-scheme-request-get-uri)

(defcfun "webkit_uri_scheme_request_get_path" :string
  (request (g-object webkit-uri-scheme-request)))
(export 'webkit-uri-scheme-request-get-path)

(defcfun "webkit_uri_scheme_request_get_web-view" (g-object webkit-web-view)
  (request (g-object webkit-uri-scheme-request)))
(export 'webkit-uri-scheme-request-get-web-view)

(defcfun "webkit_uri_scheme_request_finish" :void
  (request (g-object webkit-uri-scheme-request))
  (stream :pointer)  ; XXX: GInputStream
  (stream-length :int)
  (contents :string))
(export 'webkit-uri-scheme-request-finish)

(defcfun ("webkit_uri_scheme_request_finish_error" %webkit-uri-scheme-request-finish-error) :void
  (request (g-object webkit-uri-scheme-request))
  (g-error :pointer))

(defun webkit-uri-scheme-request-finish-error (request error-string)
  (%webkit-uri-scheme-request-finish-error
   request (glib::%g-error-new-literal
            +webkit-plugin-error+
            4
            error-string)))
(export 'webkit-uri-scheme-request-finish-error)

(defcfun "g_memory_input_stream_new_from_data" (g-object g-memory-input-stream)
  (data :pointer)
  (length :long)
  (destroy :pointer))

(cffi:defcallback uri-scheme-processed :void ((request :pointer) (user-data :pointer))
  (let ((callback (find (cffi:pointer-address user-data) callbacks :key (function callback-id))))
    (setf callbacks (delete callback callbacks))
    ;; TODO: Use `unwind-protect' to free the allocated objects?
    (when (callback-function callback)
      ;; Callback function returns data-string as a first value
      ;; and data type (e.g., "text/html") as an optional second
      ;; value.
      (destructuring-bind (data &optional (data-type "text-html"))
          (multiple-value-list (funcall (callback-function callback) request))
        (handler-case
            (multiple-value-bind (ffi-string ffi-string-length)
                (cffi:foreign-string-alloc data)
              (let* ((stream (g-memory-input-stream-new-from-data
                              ffi-string ffi-string-length (cffi:make-pointer 0))))
                (webkit-uri-scheme-request-finish request stream ffi-string-length data-type)
                (gobject:g-object-unref stream)
                (cffi:foreign-string-free ffi-string)))
          (error (c)
            (webkit-uri-scheme-request-finish-error
             request (format nil "The custom url request for URI ~S failed with ~A: ~A"
                             (webkit-uri-scheme-request-get-uri request) (type-of c) c))
            (when (and callback (callback-error-function callback))
              (funcall (callback-error-function callback) c))))))))

(defun webkit-web-context-register-uri-scheme-callback (context scheme &optional call-back error-call-back)
  (incf callback-counter)
  (push (make-callback :id callback-counter :web-view context
                       :function call-back
                       :error-function error-call-back)
        callbacks)
  (webkit-web-context-register-uri-scheme
   context scheme
   (cffi:callback uri-scheme-processed)
   (cffi:make-pointer callback-counter)
   (cffi:null-pointer)))
(export 'webkit-web-context-register-uri-scheme-callback)