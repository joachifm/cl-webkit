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

(defclass webkit-download (g-object)
  ((destination
    :allocation :gobject-property
    :g-property-name "destination"
    :g-property-type :string
    :reader download-destination)
   (estimated-progress
    :allocation :gobject-property
    :g-property-name "estimated-progress"
    :g-property-type :double
    :reader download-estimated-progress))
  (:metaclass gobject-class)
  (:g-type-name . "WebKitDownload")
  (:g-type-initializer . "webkit_download_get_type"))

(export 'webkit-download)
(export 'download-destination)
(export 'download-estimated-progress)
