;;; webkit2.uri-scheme-request.lisp --- bindings for WebKitURISchemeRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitURISchemeRequest" () ())

(defvar +webkit-plugin-error-connection-cancelled+ 4
  "A separate value for the connection cancelled plugin error for use
  in WebKitURISchemeRequest callbacks.")

(defcfun "webkit_uri_scheme_request_get_scheme" :string
  (request (g-object webkit-uri-scheme-request)))
(export 'webkit-uri-scheme-request-get-scheme)

(defcfun "webkit_uri_scheme_request_get_uri" :string
  (request (g-object webkit-uri-scheme-request)))
(export 'webkit-uri-scheme-request-get-uri)

(defcfun "webkit_uri_scheme_request_get_path" :string
  (request (g-object webkit-uri-scheme-request)))
(export 'webkit-uri-scheme-request-get-path)

(defcfun "webkit_uri_scheme_request_get_web_view" (g-object webkit-web-view)
  (request (g-object webkit-uri-scheme-request)))
(export 'webkit-uri-scheme-request-get-web-view)

(defcfun "webkit_uri_scheme_request_finish" :void
  (request (g-object webkit-uri-scheme-request))
  (stream (g-object g-memory-input-stream))
  (stream-length :long)
  (content-type :string))
(export 'webkit-uri-scheme-request-finish)

(defcfun ("webkit_uri_scheme_request_finish_error" %webkit-uri-scheme-request-finish-error) :void
  (request (g-object webkit-uri-scheme-request))
  (g-error :pointer))

(defun webkit-uri-scheme-request-finish-error (request error-string)
  (%webkit-uri-scheme-request-finish-error
   request (glib::%g-error-new-literal
            +webkit-plugin-error+
            +webkit-plugin-error-connection-cancelled+
            error-string)))
(export 'webkit-uri-scheme-request-finish-error)

(cffi:defcallback uri-scheme-processed :void ((request (g-object webkit-uri-scheme-request))
                                              (user-data :pointer))
  (g-object-ref (pointer request))
  (bt:make-thread
   (lambda ()
     (let ((callback (find (cffi:pointer-address user-data) callbacks :key (function callback-id))))
       (when (callback-function callback)
         ;; Callback function returns data-string as a first value
         ;; and data type (e.g., "text/html") as an optional second
         ;; value.
         (destructuring-bind (data &optional (data-type "text/html"))
             (multiple-value-list (funcall (callback-function callback) request))
           (handler-case
               (etypecase data
                 (string (multiple-value-bind (ffi-string ffi-string-length)
                             (cffi:foreign-string-alloc data)
                           (unwind-protect
                                (let* ((stream (g-memory-input-stream-new-from-data
                                                ffi-string -1 ; -1 is for auto-detection based on NULL character
                                                (callback g-notify-destroy-null))))
                                  (webkit-uri-scheme-request-finish request stream ffi-string-length data-type)
                                  (gobject:g-object-unref (pointer stream)))
                             (cffi:foreign-string-free ffi-string))))
                 (array (let* ((arr (cffi:foreign-alloc :uchar :initial-contents data :count (length data)))
                               (stream (g-memory-input-stream-new-from-bytes (g-bytes-new arr (length data)))))
                          (webkit-uri-scheme-request-finish request stream (length data) data-type)
                          (gobject:g-object-unref (pointer stream))
                          (cffi:foreign-free arr))))
             (error (c)
               (webkit-uri-scheme-request-finish-error
                request (format nil "The custom request for URI ~a failed with ~a: ~a"
                                (webkit-uri-scheme-request-get-uri request) (type-of c) c))
               (when (and callback (callback-error-function callback))
                 (funcall (callback-error-function callback) c))))))))))

(defun webkit-web-context-register-uri-scheme-callback (context scheme &optional call-back error-call-back)
  "Register the custom scheme.
Hide all the implementation details (callbacks, WebKit functions, C objects
allocation) from the Lisp-side.

CONTEXT is the `webkit-web-context' to register scheme for.
SCHEME is the name of the scheme as a string.
CALL-BACK is a callback of one argument -- a WebKitURISchemeRequest
for this scheme.  It should return two values: the content of the
response (as a string or byte vector) and an optional MIME
type (e.g. \"text/html\") of that response.
ERROR-CALL-BACK is the one-argument function to call on error if it
happens."
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
