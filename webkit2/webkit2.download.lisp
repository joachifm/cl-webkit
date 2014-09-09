;;; webkit2.download.lisp --- bindings for WebKitDownload

;; Copyright (c) 2014 Aaron France
;;
;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-g-object-class* "WebKitDownload" webkit-download
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "webkit_download_get_type")
  (("destination" "gchararray")
   ("estimated-progress" "gdouble")
   ("response" "WebKitURIResponse")))
