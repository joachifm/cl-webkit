;;; webkit2.web-resource.lisp --- bindings for WebKitWebResource

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitWebResource" ()
  (("response" "WebKitURIResponse")
   ("uri" "gchararray")))

(defcfun (%webkit-web-resource-get-data "webkit_web_resource_get_data") :void
  (resource (g-object webkit-web-resource))
  (cancellable :pointer) ; GCancellable
  (async-ready-callback g-async-ready-callback)
  (user-data :pointer))

(defcfun (%webkit-web-resource-get-data-finish "webkit_web_resource_get_data_finish") (:pointer :uchar)
  (resource (g-object webkit-web-resource))
  (result g-async-result)
  (length :pointer)
  (g-error :pointer))

(defun webkit-web-resource-get-data-finish (resource result length)
  (glib:with-g-error (err)
    (%webkit-web-resource-get-data-finish resource result length err)))

(cffi:defcallback resource-data-received
    :void ((source-object :pointer) (result :pointer) (user-data :pointer))
  (let ((callback (find (cffi:pointer-address user-data) callbacks :key (function callback-id))))
    (handler-case
        (let ((length (cffi:foreign-alloc :int :initial-element 0)))
          (unwind-protect
               (let ((result (webkit-web-resource-get-data-finish source-object result length)))
                 (setf callbacks (delete callback callbacks))
                 (when (callback-function callback)
                   (funcall (callback-function callback)
                            (foreign-array-to-lisp result `(:array :int ,(mem-ref length :int))))))
            (cffi:foreign-free length)))
      (condition (c)
        (when callback
          (when (callback-error-function callback)
            (funcall (callback-error-function callback) c))
          (setf callbacks (delete callback callbacks)))))))

(defun webkit-web-resource-get-data (resource &optional call-back error-call-back)
  "Get resource data."
  (incf callback-counter)
  (push (make-callback :id callback-counter
                       :function call-back
                       :error-function error-call-back)
        callbacks)
  (%webkit-web-resource-get-data
   web-view message
   (cffi:null-pointer)
   (cffi:callback resource-data-received)
   (cffi:make-pointer callback-counter)))
