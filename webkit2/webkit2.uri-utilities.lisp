;;; webkit2.uri-utilities.lisp --- bindings for WebKitURIUtilities

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(defcfun "webkit_uri_for_display" :string
  (uri :string))
(export 'webkit-uri-for-display)
