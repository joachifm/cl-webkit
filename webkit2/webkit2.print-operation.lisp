;;; webkit2.print-operation.lisp --- bindings for WebKitPrintOperation

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitPrintOperation" ()
  (("page-setup" "GtkPageSetup" t t)
   ("print-settings" "GtkPrintSettings" t t)
   ("web-view" "WebKitWebView" t t)))

(defcfun "webkit_print_operation_run_dialog" (g-object webkit-print-operation-response)
  (print-operation (g-object webkit-print-operation))
  (parent (g-object gtk:gtk-window)))
(export 'webkit-print-operation-run-dialog)

(defcfun "webkit_print_operation_print" :void
  (print-operation (g-object webkit-print-operation)))
(export 'webkit-print-operation-print)

(define-g-enum "WebKitPrintOperationResponse" webkit-print-operation-response ()
  :webkit-print-operation-response-print
  :webkit-print-operation-response-cancel)
