;;; webkit2.file-chooser-request.lisp --- bindings for WebKitFileChooserRequest

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitFileChooserRequest" () ())

(defcfun "webkit_file_chooser_request_get_mime_types" :pointer ; XXX: const gchar * const *
  (request (g-object webkit-file-chooser-request)))
(export 'webkit-file-chooser-request-get-mime-types)

(defcfun "webkit_file_chooser_request_get_mime_types_filter" :pointer ; XXX: GtkFileFilter *
  (request (g-object webkit-file-chooser-request)))
(export 'webkit-file-chooser-request-get-mime-types-filter)

(defcfun "webkit_file_chooser_request_get_select_multiple" :boolean
  (request (g-object webkit-file-chooser-request)))
(export 'webkit-file-chooser-request-get-selected-multiple)

(defcfun "webkit_file_chooser_request_select_files" :void
  (request (g-object webkit-file-chooser-request))
  (files :pointer)) ; XXX: const gchar * const *
(export 'webkit-file-chooser-request-select-files)

(defcfun "webkit_file_chooser_request_get_selected_files" :pointer ; XXX: const gchar * const *
  (request (g-object webkit-file-chooser-request)))
(export 'webkit-file-chooser-request-get-selected-files)

(defcfun "webkit_file_chooser_request_cancel" :void
  (request (g-object webkit-file-chooser-request)))
(export 'webkit-file-chooser-request-cancel)
