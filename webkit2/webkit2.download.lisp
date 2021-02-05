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

(defcfun "webkit_download_set_destination" :void
  (webkit-download (g-object webkit-download))
  (uri :string))
(export 'webkit-download-set-destination)

(defcfun "webkit_download_cancel" :void
  (webkit-download (g-object webkit-download)))
(export 'webkit-download-cancel)
