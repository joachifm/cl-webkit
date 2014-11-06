;;; webkit2.web-view.lisp --- bindings for WebKitWebView

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(defparameter +webkit-editing-command-cut+ "Cut") ; XXX: WEBKIT_EDITING_COMMAND_CUT
(defparameter +webkit-editing-command-copy+ "Copy") ; XXX: WEBKIT_EDITING_COMMAND_COPY
(defparameter +webkit-editing-command-paste+ "Paste") ; XXX: WEBKIT_EDITING_COMMAND_PASTE
(defparameter +webkit-editing-command-select-all+ "SelectAll") ; XXX: WEBKIT_EDITING_COMMAND_SELECT_ALL
(defparameter +webkit-editing-command-undo+ "Undo") ; XXX: WEBKIT_EDITING_COMMAND_UNDO
(defparameter +webkit-editing-command-redo+ "Redo") ; XXX: WEBKIT_EDITING_COMMAND_REDO

(define-webkit-class "WebKitWebView"
  (:superclass gtk-widget
   :interfaces ("AtkImplementorIface" "GtkBuildable"))
  (("estimated-load-progress" "gdouble")
   ("is-loading" "gboolean")
   ("title" "gchararray")
   ("uri" "gchararray")
   ("view-mode" "WebKitViewMode" t t)
   ("web-context" "WebKitWebContext" t t)
   ("zoom-level" "gdouble" t t)))

(defctype webkit-script-dialog :pointer) ; XXX: GBoxed WebScriptDialog

(define-g-enum "WebKitLoadEvent" webkit-load-event ()
  :webkit-load-started
  :webkit-load-redirected
  :webkit-load-committed
  :webkit-load-finished)

(define-g-enum "WebKitInsecureContentEvent" webkit-insecure-content-event ()
  :webkit-insecure-content-run
  :webkit-insecure-content-displayed)

(define-g-enum "WebKitPolicyDecisionType" webkit-policy-decision-type ()
  :webkit-policy-decision-type-navigation-action
  :webkit-policy-decision-type-new-window-action
  :webkit-policy-decision-type-response)

(define-g-enum "WebKitViewMode" webkit-view-mode ()
  :webkit-view-mode-web
  :webkit-view-mode-source)

(define-g-enum "WebKitSaveMode" webkit-save-mode ()
  :webkit-save-mode-mhtml)

(define-g-enum "WebKitSnapshotRegion" webkit-snapshot-region ()
  :webkit-snapshot-region-visible
  :webkit-snapshot-region-full-document)

(define-g-enum "WebKitSnapshotOptions" webkit-snapshot-options ()
  :webkit-snapshot-options-none
  :webkit-snapshot-options-include-selection-highlighting)

(define-g-enum "WebKitScriptDialogType" webkit-script-dialog-type ()
  :webkit-script-dialog-alert
  :webkit-script-dialog-confirm
  :webkit-script-dialog-prompt)

(defctype js-global-context-ref :pointer)

(defctype webkit-javascript-result :pointer) ; XXX: GBoxed struct

(defctype js-value-ref :pointer)

(defcfun "webkit_web_view_load_uri" :void
  (web-view (g-object webkit-web-view))
  (uri :string))
(export 'webkit-web-view-load-uri)

(defcfun "webkit_web_view_load_html" :void
  (web-view (g-object webkit-web-view))
  (content :string)
  (base-uri :string))
(export 'webkit-web-view-load-html)

(defcfun "webkit_web_view_load_alternate_html" :void
  (web-view (g-object webkit-web-view))
  (content :string)
  (content-uri :string)
  (base-uri :string)) ; XXX: can be NULL
(export 'webkit-web-view-load-alternate-html)

(defcfun "webkit_web_view_load_plain_text" :void
  (web-view (g-object webkit-web-view))
  (plain-text :string))
(export 'webkit-web-view-load-plain-text)

(defcfun "webkit_web_view_load_request" :void
  (web-view (g-object webkit-web-view))
  (request (g-object webkit-uri-request)))
(export 'webkit-web-view-load-request)

(defcfun "webkit_web_view_go_back" :void
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-go-back)

(defcfun "webkit_web_view_go_forward" :void
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-go-forward)

(defcfun "webkit_web_view_go_to_back_forward_list_item" :void
  (web-view (g-object webkit-web-view))
  (list-item (g-object webkit-back-forward-list-item)))
(export 'webkit-web-view-go-to-back-forward-list-item)

(defcfun "webkit_web_view_reload" :void
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-reload)

(defcfun "webkit_web_view_set_settings" :void
  (web-view (g-object webkit-web-view))
  (settings (g-object webkit-settings)))
(export 'webkit-web-view-set-settings)

(defcfun "webkit_web_view_get_settings" (g-object webkit-settings)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-settings)

(defcfun "webkit_web_view_get_find_controller" (g-object webkit-find-controller)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-find-controller)

(defcfun "webkit_web_view_stop_loading" :void
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-stop-loading)

(defcfun "webkit_web_view_run_javascript" :void
  (web-view (g-object webkit-web-view))
  (script :string)
  (cancellable :pointer)
  (async-ready-callback :pointer)
  (user-data :pointer))
(export 'webkit-web-view-run-javascript)

(defcfun ("webkit_web_view_run_javascript_finish" %webkit-web-view-run-javascript-finish) webkit-javascript-result
  (web-view (g-object webkit-web-view))
  (result :pointer) ; GAsyncResult
  (gerror :pointer))

(defun webkit-web-view-run-javascript-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-run-javascript-finish web-view result err)))
(export 'webkit-web-view-run-javascript-finish)

(defcfun "webkit_web_view_run_javascript_from_gresource" :void
  (web-view (g-object webkit-web-view))
  (resource :string)
  (cancellable :pointer) ; GCancellable
  (callback :pointer) ; GAsyncReadyCallback
  (user-data :pointer))
(export 'webkit-web-view-run-javascript-from-resource)

(defcfun ("webkit_web_view_run_javascript_from_gresource_finish" %webkit-web-view-run-javascript-from-gresource-finish) webkit-javascript-result
  (web-view (g-object webkit-web-view))
  (result :pointer) ; GAsyncResult
  (gerror :pointer))

(defun webkit-web-view-run-javascript-from-gresource-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-run-javascript-from-gresource-finish web-view result err)))
(export 'webkit-web-view-run-javascript-from-gresource-finish)

(defcfun "webkit_web_view_download_uri" (g-object webkit-download)
  (web-view (g-object webkit-web-view))
  (uri :string))
(export 'webkit-web-view-download-uri)

(defcfun "webkit_web_view_get_main_resource" (g-object webkit-web-resource)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-main-resource)

(defcfun "webkit_web_view_get_back_forward_list" (g-object webkit-back-forward-list)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-back-forward-list)

(defcfun "webkit_web_view_can_go_back" :boolean
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-can-go-back)

(defcfun "webkit_web_view_can_go_forward" :boolean
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-can-go-forward)

(defcfun "webkit_web_view_can_show_mime_type" :boolean
  (web-view (g-object webkit-web-view))
  (mime-type :string))
(export 'webkit-web-view-can-show-mime-type)

(defcfun "webkit_web_view_reload_bypass_cache" :void
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-reload-bypass-cache)

(defcfun "webkit_web_view_get_inspector" (g-object webkit-web-inspector)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-inspector)

(defcfun "webkit_web_view_get_tls_info" :boolean
  (web-view (g-object webkit-web-view))
  (certificate :pointer) ; XXX: GTlsCertificate **
  (errors :pointer)) ; XXX: GTlsCertificateFlags *
(export 'webkit-web-view-get-tls-info)

(defcfun "webkit_web_view_save" :void
  (web-view (g-object webkit-web-view))
  (save-mode webkit-save-mode)
  (cancellable :pointer) ; XXX: GCancellable
  (callback :pointer) ; XXX: GAsyncReadyCallback
  (user-data :pointer)) ; XXX: gpointer
(export 'webkit-web-view-save)

(defcfun ("webkit_web_view_save_finish" %webkit-web-view-save-finish) :pointer ; XXX: GInputStream
  (web-view (g-object webkit-web-view))
  (result :pointer) ; XXX: GAsyncResult
  (gerror :pointer))

(defun webkit-web-view-save-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-save-finish web-view result err)))
(export 'webkit-web-view-save-finish)

(defcfun "webkit_web_view_save_to_file" :void
  (web-view (g-object webkit-web-view))
  (file :pointer) ; XXX: GFile
  (save-mode webkit-save-mode)
  (cancellable :pointer) ; XXX: GCancellable
  (callback :pointer) ; XXX: GAsyncReadyCallback
  (user-data :pointer)) ; XXX: gpointer
(export 'webkit-web-view-save-to-file)

(defcfun ("webkit_web_view_save_to_file_finish" %webkit-web-view-save-to-file-finish) :boolean
  (web-view (g-object webkit-web-view))
  (result :pointer) ; XXX: GAsyncResult
  (gerror :pointer))

(defun webkit-web-view-save-to-file-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-save-to-file-finish web-view result err)))
(export 'webkit-web-view-save-to-file-finish)

(defcfun "webkit_web_view_get_snapshot" :void
  (web-view (g-object webkit-web-view))
  (region webkit-snapshot-region)
  (options webkit-snapshot-options)
  (cancellable :pointer) ; XXX: GCancellable
  (callback :pointer) ; XXX: GAsyncReadyCallback
  (user-data :pointer)) ; XXX: gpointer
(export 'webkit-web-view-get-snapshot)

(defcfun ("webkit_web_view_get_snapshot_finish" %webkit-web-view-get-snapshot-finish) :pointer ; XXX: cairo_surface_t
  (web-view (g-object webkit-web-view))
  (result :pointer) ; XXX: GAsyncResult
  (gerror :pointer))

(defun webkit-web-view-get-snapshot-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-get-snapshot-finish web-view result err)))
(export 'webkit-web-view-get-snapshot-finish)

(defcfun "webkit_script_dialog_get_dialog_type" webkit-script-dialog-type
  (dialog webkit-script-dialog))
(export 'webkit-script-dialog-get-dialog-type)

(defcfun "webkit_script_dialog_get_message" :string
  (dialog (g-object webkit-script-dialog)))
(export 'webkit-script-dialog-get-message)

(defcfun "webkit_script_dialog_confirm_set_confirmed" :void
  (dialog (g-object webkit-script-dialog))
  (confirmed :boolean))
(export 'webkit-script-dialog-set-confirmed)

(defcfun "webkit_script_dialog_prompt_get_default_text" :string
  (dialog (g-object webkit-script-dialog)))
(export 'webkit-script-dialog-prompt-get-default-text)

(defcfun "webkit_script_dialog_prompt_set_text" :void
  (dialog (g-object webkit-script-dialog))
  (text :string))
(export 'webkit-script-dialog-prompt-set-text)

(defcfun "webkit_web_view_can_execute_editing_command" :void
  (web-view (g-object webkit-web-view))
  (command :string)
  (cancellable :pointer) ; XXX: GCanellable
  (callback :pointer) ; XXX: GAsyncReadyCallback
  (user-data :pointer)) ; XXX: gpointer
(export 'webkit-web-view-can-execute-editing-command)

(defcfun ("webkit_web_view_can_execute_editing_command_finish" %webkit-web-view-can-execute-editing-command-finish) :boolean
  (web-view (g-object webkit-web-view))
  (result :pointer) ; XXX: GAsyncResult
  (gerror :pointer))

(defun webkit-web-view-can-execute-editing-command-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-can-execute-editing-command-finish web-view result err)))
(export 'webkit-web-view-can-execute-editing-command-finish)

(defcfun "webkit_web_view_execute_editing_command" :void
  (web-view (g-object webkit-web-view))
  (command :string))
(export 'webkit-web-view-can-execute-editing-command)

(defcfun "webkit_web_view_get_javascript_global_context" js-global-context-ref
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-javascript-global-context)

(defcfun "webkit_javascript_result_ref" webkit-javascript-result
  (js-result webkit-javascript-result))
(export 'webkit-javascript-result-ref)

(defcfun "webkit_javascript_result_unref" :void
  (js-result webkit-javascript-result))
(export 'webkit-javascript-result-unref)

(defcfun "webkit_javascript_result_get_global_context" js-global-context-ref
  (js-result webkit-javascript-result))
(export 'webkit-javascript-result-get-global-context)

(defcfun "webkit_javascript_result_get_value" js-value-ref
  (js-result webkit-javascript-result))
(export 'webkit-javascript-result-get-value)
