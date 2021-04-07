;;; webkit2.download.lisp --- bindings for WebKitDownload

;; Copyright (c) 2014 Aaron France
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitDownload" ()
  (("destination" "gchararray")
   ("estimated-progress" "gdouble")
   ("response" "WebKitURIResponse")
   ("allow-overwrite" "gboolean")))

(defcfun "webkit_download_get_request" (g-object webkit-uri-request)
  (webkit-download (g-object webkit-download)))
(export 'webkit-download-get-request)

(defcfun "webkit_download_get_destination" :pointer ; const gchar*
  (webkit-download (g-object webkit-download)))
(export 'webkit-download-get-destination)

(defcfun "webkit_download_set_destination" :void
  (webkit-download (g-object webkit-download))
  (uri :string))
(export 'webkit-download-set-destination)

(defcfun "webkit_download_get_response" (g-object webkit-uri-request)
  (webkit-download (g-object webkit-download)))
(export 'webkit-download-get-response)

(defcfun "webkit_download_cancel" :void
  (webkit-download (g-object webkit-download)))
(export 'webkit-download-cancel)

(defcfun "webkit_download_get_estimated_progress" :double
  (webkit-download (g-object webkit-download)))
(export 'webkit-download-get-estimated-progress)

(defcfun "webkit_download_get_elapsed_time" :double
  (webkit-download (g-object webkit-download)))
(export 'webkit-download-get-elapsed-time)

(defcfun "webkit_download_get_received_data_length" :uint ; guint64
  (webkit-download (g-object webkit-download)))
(export 'webkit-download-get-received-data-length)

(defcfun "webkit_download_get_web_view" (g-object webkit-web-view)
  (webkit-download (g-object webkit-download)))
(export 'webkit-download-get-web-view)

(defcfun "webkit_download_get_allow_overwrite" :boolean
  (webkit-download (g-object webkit-download)))
(export 'webkit-download-get-allow-overwrite)

(defcfun "webkit_download_set_allow_overwrite" :void
  (webkit-download (g-object webkit-download))
  (allowed :boolean))
(export 'webkit-download-set-allow-overwrite)
