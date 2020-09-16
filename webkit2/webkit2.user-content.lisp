;;; webkit2.user-content.lisp --- bindings for WebKit-usable user content

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(defctype webkit-user-style-sheet :pointer) ;; GBoxed struct WebKitUserStyleSheet

(defctype webkit-user-script :pointer) ;; GBoxed struct WebKitUserScript

(defctype webkit-user-content-filter :pointer) ;; GBoxed struct WebKitUserContentFilter

(define-g-enum "WebKitUserContentInjectedFrames" webkit-user-content-injected-frames ()
  :webkit-user-content-inject-all-frames
  :webkit-user-content-inject-top-frame)

(define-g-enum "WebKitUserStyleLevel" webkit-user-style-level ()
  :webkit-user-style-level-user
  :webkit-user-style-level-author)

(define-g-enum "WebKitUserScriptInjectionTime" webkit-user-script-injection-time ()
  :webkit-user-script-inject-at-document-start
  :webkit-user-script-inject-at-document-end)

(defcfun "webkit_user_style_sheet_ref" webkit-user-style-sheet
  (user-style-sheet webkit-user-style-sheet))
(export 'webkit-user-style-sheet-ref)

(defcfun "webkit_user_style_sheet_unref" :void
  (user-style-sheet webkit-user-style-sheet))
(export 'webkit-user-style-sheet-unref)

(defcfun "webkit_user_style_sheet_new" webkit-user-style-sheet
  (source :string)
  (injected-frames webkit-user-content-injected-frames)
  (level webkit-user-style-level)
  (allow-list :pointer) ;; const gchar * const *
  (block-list :pointer)) ;; const gchar * const *
(export 'webkit-user-style-sheet-new)

(defcfun "webkit_user_style_sheet_new_for_world" webkit-user-style-sheet
  (source :string)
  (injected-frames webkit-user-content-injected-frames)
  (level webkit-user-style-level)
  (world-name :string)
  (allow-list :pointer) ;; const gchar * const *
  (block-list :pointer)) ;; const gchar * const *
(export 'webkit-user-style-sheet-new-for-world)

(defcfun "webkit_user_script_ref" webkit-user-script
  (user-script webkit-user-script))
(export 'webkit-user-script-ref)

(defcfun "webkit_user_script_unref" :void
  (user-script webkit-user-script))
(export 'webkit-user-script-unref)

(defcfun "webkit_user_script_new" webkit-user-script
  (source :string)
  (injected-frames webkit-user-content-injected-frames)
  (injection-time webkit-user-script-injection-time)
  (allow-list :pointer) ;; const gchar * const *
  (block-list :pointer)) ;; const gchar * const *
(export 'webkit-user-script-new)

(defcfun "webkit_user_script_new_for_world" webkit-user-script
  (source :string)
  (injected-frames webkit-user-content-injected-frames)
  (injection-time webkit-user-script-injection-time)
  (world-name :string)
  (allow-list :pointer) ;; const gchar * const *
  (block-list :pointer)) ;; const gchar * const *
(export 'webkit-user-script-new-for-world)

(defcfun "webkit_user_content_filter_ref" webkit-user-content-filter
  (user-content-filter webkit-user-content-filter))
(export 'webkit-user-content-filter-ref)

(defcfun "webkit_user_content_filter_unref" :void
  (user-content-filter webkit-user-content-filter))
(export 'webkit-user-content-filter-unref)

(defcfun "webkit_user_content_filter_get_identifier" :pointer ;; const char *
  (user-content-filter webkit-user-content-filter))
(export 'webkit-user-content-filter-get-identifier)
