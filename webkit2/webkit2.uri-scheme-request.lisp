;;; webkit2.uri-scheme-request.lisp --- bindings for WebKitURISchemeRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(defctype webkit-uri-scheme-request :pointer)

(defcfun "webkit_uri_scheme_request_get_scheme" :string
  (request webkit-uri-scheme-request))
(export 'webkit-uri-scheme-request-get-scheme)

(defcfun "webkit_uri_scheme_request_get_uri" :string
  (request webkit-uri-scheme-request))
(export 'webkit-uri-scheme-request-get-uri)

(defcfun "webkit_uri_scheme_request_get_path" :string
  (request webkit-uri-scheme-request))
(export 'webkit-uri-scheme-request-get-path)

(defcfun "webkit_uri_scheme_request_get_web-view" (g-object webkit-web-view)
  (request webkit-uri-scheme-request))
(export 'webkit-uri-scheme-request-get-web-view)

(defcfun "webkit_uri_scheme_request_finish" :void
  (request webkit-uri-scheme-request)
  (stream :pointer)  ; XXX: GInputStream
  (stream-length :int)
  (contents :string))
(export 'webkit-uri-scheme-request-finish)

(defcfun ("webkit_uri_scheme_request_finish_error" %webkit-uri-scheme-request-finish-error) :void
  (request webkit-uri-scheme-request)
  (g-error :pointer))

(defun webkit-uri-scheme-request-finish-error (request)
  (glib:with-g-error (err)
    (%webkit-uri-scheme-request-finish-error request err)))
(export 'webkit-uri-scheme-request-finish-error)

(cffi:defcallback uri-scheme-processed :void ((request :pointer) (user-data :pointer))
  (let ((callback (find (cffi:pointer-address user-data) callbacks :key (function callback-id))))
    (handler-case
        (progn
          (setf callbacks (delete callback callbacks))
          (when (callback-function callback)
            (funcall (callback-function callback) request)))
      (error (c)
        (when callback
          (when  (callback-error-function callback)
            (funcall (callback-error-function callback) c))
          (setf callbacks (delete callback callbacks)))))))

(defun webkit-web-context-register-uri-scheme-callback (context scheme &optional call-back error-call-back)
  (incf callback-counter)
  (push (make-callback :id callback-counter :data context
                       :function call-back
                       :error-function error-call-back)
        callbacks)
  (webkit-web-context-register-uri-scheme
   context scheme
   (cffi:callback uri-scheme-processed)
   (cffi:make-pointer callback-counter)
   (cffi:null-pointer)))
