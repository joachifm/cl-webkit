;;; webkit2.user-content-filter-store.lisp --- bindings for WebKitUserContentFilterStore

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitUserContentFilterStore" () ())

(defcfun "webkit_user_content_filter_store_new" (g-object webkit-user-content-filter-store)
  (storage-path :string))
(export 'webkit-user-content-filter-store-new)

(defcfun "webkit_user_content_filter_store_fetch_identifiers" :void
  (store (g-object webkit-user-content-filter-store))
  (cancellable :pointer)                ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-user-content-filter-store-fetch-identifiers)

(defcfun "webkit_user_content_filter_store_fetch_identifiers_finish" (:pointer :string)
  (store (g-object webkit-user-content-filter-store))
  (result g-async-result))
(export 'webkit-user-content-filter-store-fetch-identifiers-finish)

(defcfun "webkit_user_content_filter_store_get_path" :string
  (store (g-object webkit-user-content-filter-store)))
(export 'webkit-user-content-filter-store-get-path)

(defcfun "webkit_user_content_filter_store_load" :void
  (store (g-object webkit-user-content-filter-store))
  (identifier :string)
  (cancellable :pointer)                ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-user-content-filter-store-load)

(defcfun ("webkit_user_content_filter_store_load_finish" %webkit-user-content-filter-store-load-finish)
    (g-object webkit-user-content-filter)
  (store (g-object webkit-user-content-filter-store))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-user-content-filter-store-load-finish (store result)
  (glib:with-g-error (err)
    (%webkit-user-content-filter-store-load-finish store result err)))
(export 'webkit-user-content-filter-store-load-finish)

(defcfun "webkit_user_content_filter_store_remove" :void
  (store (g-object webkit-user-content-filter-store))
  (identifier :string)
  (cancellable :pointer)                ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-user-content-filter-store-remove)

(defcfun ("webkit_user_content_filter_store_remove_finish" %webkit-user-content-filter-store-remove-finish) :bool
  (store (g-object webkit-user-content-filter-store))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-user-content-filter-store-remove-finish (store result)
  (glib:with-g-error (err)
    (%webkit-user-content-filter-store-remove-finish store result err)))
(export 'webkit-user-content-filter-store-remove-finish)

(defcfun "webkit_user_content_filter_store_save" :void
  (store (g-object webkit-user-content-filter-store))
  (identifier :string)
  (source :pointer) ; GBytes
  (cancellable :pointer)                ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-user-content-filter-store-save)

(defcfun ("webkit_user_content_filter_store_save_finish" %webkit-user-content-filter-store-save-finish)
    (g-object webkit-user-content-filter)
  (store (g-object webkit-user-content-filter-store))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-user-content-filter-store-save-finish (store result)
  (glib:with-g-error (err)
    (%webkit-user-content-filter-store-save-finish store result err)))
(export 'webkit-user-content-filter-store-save-finish)

(defcfun "webkit_user_content_filter_store_save_from_file" :void
  (store (g-object webkit-user-content-filter-store))
  (identifier :string)
  (file (g-object gio:g-file))
  (cancellable :pointer)                ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-user-content-filter-store-save-from-file)

(defcfun ("webkit_user_content_filter_store_save_from_file_finish" %webkit-user-content-filter-store-save-from-file-finish)
    (g-object webkit-user-content-filter)
  (store (g-object webkit-user-content-filter-store))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-user-content-filter-store-save-from-file-finish (store result)
  (glib:with-g-error (err)
    (%webkit-user-content-filter-store-save-from-file-finish store result err)))
(export 'webkit-user-content-filter-store-save-from-file-finish)
