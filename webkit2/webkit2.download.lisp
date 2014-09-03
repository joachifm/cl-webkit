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

(defclass webkit-download-class (g-object)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "WebKitDownload"))

(export 'webkit-download-class)

(defctype webkit-download :pointer)
(export 'webkit-download)
