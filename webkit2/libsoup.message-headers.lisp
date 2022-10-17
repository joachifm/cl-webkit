;;; libsoup.message-headers.lisp --- bindings for SoupMessageHeaders

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-g-enum "SoupMessageHeadersType" soup-message-headers-type ()
  :soup-message-headers-request
  :soup-message-headers-response
  :soup-message-headers-multipart)

(define-g-enum "SoupEncoding" soup-encoding ()
  :soup-encoding-unrecognized
  :soup-encoding-none
  :soup-encoding-content-length
  :soup-encoding-eof
  :soup-encoding-chunked
  :soup-encoding-byteranges)

(define-g-enum "SoupExpectation" soup-expectation ()
  :soup-expectation-unrecognized
  :soup-expectation-continue)

(defcstruct soup-range
  (start :uint)
  (start :uint))

(defcstruct soup-message-headers)

(defcfun "soup_message_headers_new" (:pointer (:struct soup-message-headers))
  (type soup-message-headers-type))
(export 'soup-message-headers-new)

(defcfun "soup_message_headers_free" :void
  (headers (:pointer (:struct soup-message-headers))))
(export 'soup-message-headers-free)

(defcfun "soup_message_headers_append" :void
  (headers (:pointer (:struct soup-message-headers)))
  (name :string)
  (value :string))
(export 'soup-message-headers-append)

(defcfun "soup_message_headers_replace" :void
  (headers (:pointer (:struct soup-message-headers)))
  (name :string)
  (value :string))
(export 'soup-message-headers-replace)

(defcfun "soup_message_headers_remove" :void
  (headers (:pointer (:struct soup-message-headers)))
  (name :string))
(export 'soup-message-headers-remove)

(defcfun "soup_message_headers_clear" :void
  (headers (:pointer (:struct soup-message-headers))))
(export 'soup-message-headers-clear)

(defcfun "soup_message_headers_clean_connection_headers" :void
  (headers (:pointer (:struct soup-message-headers))))
(export 'soup-message-headers-clean-connection-headers)

(defcfun "soup_message_headers_get_one" :string
  (headers (:pointer (:struct soup-message-headers)))
  (name :string))
(export 'soup-message-headers-clean-get-one)

(defcfun "soup_message_headers_get_list" :string
  (headers (:pointer (:struct soup-message-headers)))
  (name :string))
(export 'soup-message-headers-clean-get-list)

(defcfun "soup_message_headers_get_headers_type" soup-message-headers-type
  (headers (:pointer (:struct soup-message-headers))))
(export 'soup-message-headers-clean-get-headers-type)

(defcfun "soup_message_headers_header_contains" :bool
  (headers (:pointer (:struct soup-message-headers)))
  (name :string)
  (token :string))
(export 'soup-message-headers-header-contains)

(defcfun "soup_message_headers_header_equals" :bool
  (headers (:pointer (:struct soup-message-headers)))
  (name :string)
  (value :string))
(export 'soup-message-headers-header-equals)

(defcfun "soup_message_headers_get_encoding" soup-encoding
  (headers (:pointer (:struct soup-message-headers))))
(export 'soup-message-headers-get-encoding)

(defcfun "soup_message_headers_set_encoding" :void
  (headers (:pointer (:struct soup-message-headers)))
  (encoding soup-encoding))
(export 'soup-message-headers-set-encoding)

(defcfun "soup_message_headers_get_content_length" :uint
  (headers (:pointer (:struct soup-message-headers))))
(export 'soup-message-headers-get-content-length)

(defcfun "soup_message_headers_set_content_length" :void
  (headers (:pointer (:struct soup-message-headers)))
  (content-length :uint))
(export 'soup-message-headers-set-content-length)

(defcfun "soup_message_headers_get_expectations" :uint
  (headers (:pointer (:struct soup-message-headers))))
(export 'soup-message-headers-get-expectations)

(defcfun "soup_message_headers_set_expectations" :void
  (headers (:pointer (:struct soup-message-headers)))
  (expectations :uint))
(export 'soup-message-headers-set-expectations)

(defcfun ("soup_message_headers_get_content_type"
          %soup-message-headers-get-content-type)
    :string
  (headers (:pointer (:struct soup-message-headers)))
  (params :pointer)) ;; XXX: GHashTable, not bound in cl-cffi-gtk?

(defun soup-message-headers-get-content-type (headers)
  (%soup-message-headers-get-content-type headers (cffi:null-pointer)))
(export 'soup-message-headers-get-content-type)

(defcfun ("soup_message_headers_set_content_type"
          %soup-message-headers-set-content-type)
    :void
  (headers (:pointer (:struct soup-message-headers)))
  (content-type :uint)
  (params :pointer))

(defun soup-message-headers-set-content-type (headers content-type)
  (%soup-message-headers-set-content-type headers content-type (cffi:null-pointer)))
(export 'soup-message-headers-set-content-type)

(defcfun "soup_message_headers_foreach" :void
  (headers (:pointer (:struct soup-message-headers)))
  (function :pointer)
  (user-data :pointer))

(defvar *header-names* nil)

(cffi:defcallback collect-header-names :void ((name :string) (value :string) (user-data :pointer))
  (declare (ignore user-data))
  (pushnew (cons name value) *header-names*
           :test #'string-equal :key #'first))

(defun soup-message-headers-get-headers (headers)
  "List all the header names accessible in HEADERS.
Return an alist of headers and their values."
  ;; FIXME: Do CFFI callbacks take lexical scope into account? Does
  ;; that prevent them from being GC-ed?
  (let ((*header-names* nil))
    (soup-message-headers-foreach
     headers (cffi:callback collect-header-names) (cffi:null-pointer))
    *header-names*))
(export 'soup-message-headers-get-headers)
