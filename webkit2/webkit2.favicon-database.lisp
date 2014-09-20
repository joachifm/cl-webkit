;;; webkit2.favicon-database.lisp --- bindings for WebKitFaviconDatabase

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitFaviconDatabase" () ())

(define-g-enum "WebKitFaviconDatabaseError" webkit-favicon-database-error ()
  :webkit-favicon-database-error-not-initialized
  :webkit-favicon-database-error-favicon-not-found
  :webkit-favicon-database-error-favicon-unknown)
