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
     ,@body))

(export 'with-g-async-ready-callback)
(defmacro with-g-async-ready-callback ((var callback) &body body)
  "Example:

\(let ((result-channel (make-instance 'calispel:channel)))
  (bt:make-thread
   (gtk:within-gtk-thread
     (let* ((context (webkit:webkit-web-view-web-context view))
            (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
       (webkit:with-g-async-ready-callback (callback (lambda (source result user-data)
                                                       (declare (ignore source user-data))
                                                       (calispel:! result-channel
                                                                   (webkit:webkit-cookie-manager-get-accept-policy-finish
                                                                    cookie-manager
                                                                    result))))
         (webkit:webkit-cookie-manager-get-accept-policy
          cookie-manager
          (cffi:null-pointer)
          callback
          (cffi:null-pointer))))))
  (calispel:? result-channel))"
  (let ((g (gensym "CALLBACK")))
    `(progn
       (define-g-async-ready-callback ,g
         (funcall ,callback source-object result user-data))
       (let ((,var (callback ,g)))
         ,@body)
       (fmakunbound ',g))))
