;;; webkit2.web-view.lisp --- bindings for WebKitWebView

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

;; XXX: grovel these
(defparameter +webkit-editing-command-cut+ "Cut") ; XXX: WEBKIT_EDITING_COMMAND_CUT
(export '+webkit-editing-command-cut+)

(defparameter +webkit-editing-command-copy+ "Copy") ; XXX: WEBKIT_EDITING_COMMAND_COPY
(export '+webkit-editing-command-copy+)

(defparameter +webkit-editing-command-paste+ "Paste") ; XXX: WEBKIT_EDITING_COMMAND_PASTE
(export '+webkit-editing-command-paste+)

;; XXX: WEBKIT_EDITING_COMMAND_PASTE_AS_PLAIN_TEXT
#+webkit2-paste-plaintext
(defparameter +webkit-editing-command-paste-as-plain-text+ "PasteAsPlainText")
#+webkit2-paste-plaintext
(export '+webkit-editing-command-paste-as-plain-text+)

(defparameter +webkit-editing-command-select-all+ "SelectAll") ; XXX: WEBKIT_EDITING_COMMAND_SELECT_ALL
(export '+webkit-editing-command-select-all+)

(defparameter +webkit-editing-command-undo+ "Undo") ; XXX: WEBKIT_EDITING_COMMAND_UNDO
(export '+webkit-editing-command-undo+)

(defparameter +webkit-editing-command-redo+ "Redo") ; XXX: WEBKIT_EDITING_COMMAND_REDO
(export '+webkit-editing-command-redo+)

(defparameter +webkit-editing-command-insert-image+ "InsertImage") ; XXX: WEBKIT_EDITING_COMMAND_INSERT_IMAGE
(export '+webkit-editing-command-insert-image+)

(defparameter +webkit-editing-command-create-link+ "CreateLink") ; XXX: WEBKIT_EDITING_COMMAND_CREATE_LINK
(export '+webkit-editing-command-create-link+)

(define-g-enum "WebKitAutomationBrowsingContextPresentation" webkit-automation-browsing-context-presentation ()
  :web-kit-automation-browsing-context-presentation-window
  :web-kit-automation-browsing-context-presentation-tab)

(define-g-enum "WebKitMediaCaptureState" webkit-media-capture-state ()
  :webkit-media-capture-state-none
  :webkit-media-capture-state-active
  :webkit-media-capture-state-muted)

(define-g-enum "WebKitWebExtensionMode" webkit-web-extension-mode ()
  :webkit-web-extension-mode-none
  :webkit-web-extension-mode-manifest-v2
  :webkit-web-extension-mode-manifest-v3)

(define-webkit-class "WebKitWebView"
    (:superclass gtk-widget
     :interfaces ("AtkImplementorIface" "GtkBuildable"))
    (("automation-presentation-type" "WebKitAutomationBrowsingContextPresentation")
     ("default-content-security-policy" "gchararray")
     ("camera-capture-state" "WebKitMediaCaptureState" t t)
     ("display-capture-state" "WebKitMediaCaptureState" t t)
     ("microphone-capture-state" "WebKitMediaCaptureState" t t)
     ("editable" "gboolean")
     ("estimated-load-progress" "gdouble")
     ("favicon" "gpointer")
     ("is-controlled-by-automation" "gboolean")
     ("is-ephemeral" "gboolean")
     ("is-loading" "gboolean")
     #+webkit2-mute
     ("is-muted" "gboolean")
     ("is-playing-audio" "gboolean")
     ("is-web-process-responsive" "gboolean")
     ("page-id" "guint64")
     ("settings" "WebKitSettings")
     ("title" "gchararray")
     ("uri" "gchararray")
     ("user-content-manager" "WebKitUserContentManager")
     ("web-context" "WebKitWebContext" t t)
     ("web-extension-mode" "WebKitWebExtensionMode")
     ;; TODO: "website-policies"
     ("zoom-level" "gdouble" t t)))

(defctype webkit-script-dialog :pointer) ; XXX: GBoxed WebScriptDialog

(define-g-boxed-opaque webkit-script-dialog "WebKitScriptDialog"
  :alloc (error "WebKitScriptDialog can not be created from Lisp side."))

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

(define-g-enum "WebKitWebProcessTerminationReason" webkit-web-process-termination-reason ()
  :webkit-web-process-crashed
  :webkit-web-process-exceeded-memory-limit)

(define-g-enum "WebKitSnapshotOptions" webkit-snapshot-options ()
  :webkit-snapshot-options-none
  :webkit-snapshot-options-include-selection-highlighting)

(define-g-enum "WebKitScriptDialogType" webkit-script-dialog-type ()
  :webkit-script-dialog-alert
  :webkit-script-dialog-confirm
  :webkit-script-dialog-prompt
  :webkit-script-dialog-before-unload-confirm)

(defctype js-global-context-ref :pointer)

(defctype webkit-javascript-result :pointer) ; XXX: GBoxed struct

(defctype js-value-ref :pointer)

(defcfun "webkit_web_view_new_with_user_content_manager" (g-object webkit-web-view)
  (user-content-manager (g-object webkit-user-content-manager)))
(export 'webkit-web-view-new-with-user-content-manager)

(defcfun "webkit_web_view_get_user_content_manager" (g-object webkit-user-content-manager)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-user-content-manager)

(defcfun "webkit_web_view_get_website_data_manager" (g-object webkit-website-data-manager)
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-website-data-manager)

(defcfun "webkit_web_view_get_favicon" :pointer
  (web-view (g-object webkit-web-view)))
(export 'webkit-web-view-get-favicon)

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
  (base-uri :string))
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

(defcfun "webkit_web_view_load_bytes" :void
  (web-view (g-object webkit-web-view))
  (bytes :pointer) ; XXX: GBytes *
  (mime-type :string)
  (encoding :string)
  (base-uri :string))
(export 'webkit-web-view-load-bytes)

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

#+webkit2-mute
(defcfun "webkit_web_view_set_is_muted" :void
  (web-view (g-object webkit-web-view))
  (muted :boolean))
#+webkit2-mute
(export 'webkit-web-view-set-is-muted)

#+webkit2-mute
(defcfun "webkit_web_view_get_is_muted" :boolean
  (web-view (g-object webkit-web-view)))
#+webkit2-mute
(export 'webkit-web-view-get-is-muted)

(defcfun "webkit_web_view_run_javascript" :void
  (web-view (g-object webkit-web-view))
  (script :string)
  (cancellable :pointer)
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-web-view-run-javascript)

(defcfun "webkit_web_view_run_javascript_in_world" :void
  (web-view (g-object webkit-web-view))
  (script :string)
  (world-name :string)
  (cancellable :pointer)
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-web-view-run-javascript-in-world)

(cffi:defcallback javascript-evaluation-complete
    :void ((source-object :pointer) (result :pointer) (user-data :pointer))
  (declare (ignore source-object))
  (let ((callback (find (cffi:pointer-address user-data) callbacks :key (function callback-id))))
    (handler-case
        (let* ((js-result (webkit-web-view-run-javascript-finish (callback-web-view callback) result))
               (value (webkit-javascript-result-get-js-value js-result))
               (exception (jsc-context-get-exception (jsc-value-get-context value))))
          (when exception
            (signal 'jsc-exception-condition :exception exception))
          (setf callbacks (delete callback callbacks))
          (when (callback-function callback)
            (funcall (callback-function callback) (jsc-value-to-lisp value) value))
          (webkit-javascript-result-unref js-result))
      (condition (c)
        (when callback
          (when  (callback-error-function callback)
            ;; We don't ignore errors when running the callback: this way the
            ;; caller can run code that can (possibly intentionally) raise a
            ;; condition.  It's up to the caller to make the error callback
            ;; condition-less or not.
            (funcall (callback-error-function callback) c))
          (setf callbacks (delete callback callbacks)))))))

(declaim (ftype (function (webkit-web-view string &optional
                                           (or null (function (t t)))
                                           (or null (function (condition)))
                                           (or null string)))
                webkit-web-view-evaluate-javascript))
(defun webkit-web-view-evaluate-javascript (web-view javascript
                                            &optional call-back error-call-back world)
  "Evaluate JAVASCRIPT in WEB-VIEW (and JavaScript world named WORLD, if present) calling CALL-BACK upon completion.
CALL-BACK is called over two arguments:
- The Lisp transformation of the result.
- The untransformed result (a JSCValue).
ERROR-CALL-BACK is called with the signaled condition."
  (incf callback-counter)
  (push (make-callback :id callback-counter :web-view web-view
                       :function call-back
                       :error-function error-call-back)
        callbacks)
  (if world
      (webkit-web-view-run-javascript-in-world
       web-view javascript world
       (cffi:null-pointer)
       (cffi:callback javascript-evaluation-complete)
       (cffi:make-pointer callback-counter))
      (webkit-web-view-run-javascript
       web-view javascript
       (cffi:null-pointer)
       (cffi:callback javascript-evaluation-complete)
       (cffi:make-pointer callback-counter))))
(export 'webkit-web-view-evaluate-javascript)

(defcfun ("webkit_web_view_run_javascript_finish" %webkit-web-view-run-javascript-finish) webkit-javascript-result
  (web-view (g-object webkit-web-view))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-web-view-run-javascript-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-run-javascript-finish web-view result err)))
(export 'webkit-web-view-run-javascript-finish)

(defcfun "webkit_web_view_run_javascript_from_gresource" :void
  (web-view (g-object webkit-web-view))
  (resource :string)
  (cancellable :pointer) ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-web-view-run-javascript-from-gresource)

(defcfun ("webkit_web_view_run_javascript_from_gresource_finish" %webkit-web-view-run-javascript-from-gresource-finish) webkit-javascript-result
  (web-view (g-object webkit-web-view))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-web-view-run-javascript-from-gresource-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-run-javascript-from-gresource-finish web-view result err)))
(export 'webkit-web-view-run-javascript-from-gresource-finish)

(defun webkit-web-view-evaluate-javascript-from-gresource (web-view resource &optional call-back error-call-back)
  "Evaluate JavaScript from RESOURCE in WEB-VIEW calling CALL-BACK upon completion and ERROR-CALL-BACK on error."
  (incf callback-counter)
  (push (make-callback :id callback-counter :web-view web-view
                       :function call-back
                       :error-function error-call-back)
        callbacks)
  (webkit-web-view-run-javascript-from-gresource
   web-view resource
   (cffi:null-pointer)
   (cffi:callback javascript-evaluation-complete)
   (cffi:make-pointer callback-counter)))
(export 'webkit-web-view-evaluate-javascript-from-gresource)

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
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-web-view-save)

(defcfun ("webkit_web_view_save_finish" %webkit-web-view-save-finish) :pointer ; XXX: GInputStream
  (web-view (g-object webkit-web-view))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-web-view-save-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-save-finish web-view result err)))
(export 'webkit-web-view-save-finish)

(defcfun "webkit_web_view_save_to_file" :void
  (web-view (g-object webkit-web-view))
  (file :pointer) ; XXX: GFile
  (save-mode webkit-save-mode)
  (cancellable :pointer) ; XXX: GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-web-view-save-to-file)

(defcfun ("webkit_web_view_save_to_file_finish" %webkit-web-view-save-to-file-finish) :boolean
  (web-view (g-object webkit-web-view))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-web-view-save-to-file-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-save-to-file-finish web-view result err)))
(export 'webkit-web-view-save-to-file-finish)

(defcfun "webkit_web_view_get_snapshot" :void
  (web-view (g-object webkit-web-view))
  (region webkit-snapshot-region)
  (options webkit-snapshot-options)
  (cancellable :pointer) ; XXX: GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-web-view-get-snapshot)

(defcfun ("webkit_web_view_get_snapshot_finish" %webkit-web-view-get-snapshot-finish) :pointer ; XXX: cairo_surface_t
  (web-view (g-object webkit-web-view))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-web-view-get-snapshot-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-get-snapshot-finish web-view result err)))
(export 'webkit-web-view-get-snapshot-finish)

(defcfun "webkit_script_dialog_ref" :void
  (dialog webkit-script-dialog))
(export 'webkit-script-dialog-ref)

(defcfun "webkit_script_dialog_unref" :void
  (dialog webkit-script-dialog))
(export 'webkit-script-dialog-unref)

(defcfun "webkit_script_dialog_get_dialog_type" webkit-script-dialog-type
  (dialog webkit-script-dialog))
(export 'webkit-script-dialog-get-dialog-type)

(defcfun "webkit_script_dialog_get_message" :string
  (dialog webkit-script-dialog))
(export 'webkit-script-dialog-get-message)

(defcfun "webkit_script_dialog_confirm_set_confirmed" :void
  (dialog webkit-script-dialog)
  (confirmed :boolean))
(export 'webkit-script-dialog-confirm-set-confirmed)

(defcfun "webkit_script_dialog_prompt_get_default_text" :string
  (dialog webkit-script-dialog))
(export 'webkit-script-dialog-prompt-get-default-text)

(defcfun "webkit_script_dialog_prompt_set_text" :void
  (dialog webkit-script-dialog)
  (text :string))
(export 'webkit-script-dialog-prompt-set-text)

(defcfun "webkit_script_dialog_close" :void
  (dialog webkit-script-dialog))
(export 'webkit-script-dialog-close)

(defcfun ("webkit_web_view_can_execute_editing_command" %webkit-web-view-can-execute-editing-command) :void
  (web-view (g-object webkit-web-view))
  (command :string)
  (cancellable :pointer) ; XXX: GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))

(defcfun ("webkit_web_view_can_execute_editing_command_finish" %webkit-web-view-can-execute-editing-command-finish) :boolean
  (web-view (g-object webkit-web-view))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-web-view-can-execute-editing-command-finish (web-view result)
  (glib:with-g-error (err)
    (%webkit-web-view-can-execute-editing-command-finish web-view result err)))
(export 'webkit-web-view-can-execute-editing-command-finish)

(defcfun "webkit_web_view_execute_editing_command" :void
  (web-view (g-object webkit-web-view))
  (command :string))
(export 'webkit-web-view-execute-editing-command)

(defcfun "webkit_web_view_execute_editing_command_with_argument" :void
  (web-view (g-object webkit-web-view))
  (command :string)
  (argument :string))
(export 'webkit-web-view-execute-editing-command-with-arguments)

(cffi:defcallback can-execute-command-checked
    :void ((source-object :pointer) (result :pointer) (user-data :pointer))
  (let ((callback (find (cffi:pointer-address user-data) callbacks :key (function callback-id))))
    (handler-case
        (let ((reply (webkit-web-view-can-execute-editing-command-finish source-object result)))
          (setf callbacks (delete callback callbacks))
          (when (callback-function callback)
            (funcall (callback-function callback) reply)))
      (condition (c)
        (when callback
          (when (callback-error-function callback)
            (funcall (callback-error-function callback) c))
          (setf callbacks (delete callback callbacks)))))))

(defun webkit-web-view-can-execute-editing-command (web-view command &optional call-back error-call-back)
  "Execute the editing command in the WEB-VIEW calling CALL-BACK upon completion and ERROR-CALL-BACK on error."
  (incf callback-counter)
  (push (make-callback :id callback-counter :web-view web-view
                       :function call-back
                       :error-function error-call-back)
        callbacks)
  (%webkit-web-view-can-execute-editing-command
   web-view command
   (cffi:null-pointer)
   (cffi:callback can-execute-command-checked)
   (cffi:make-pointer callback-counter)))
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

;; Deprecated, use webkit-javascript-result-get-js-value instead.
(defcfun "webkit_javascript_result_get_value" js-value-ref
  (js-result webkit-javascript-result))
(export 'webkit-javascript-result-get-value)

(defcfun "webkit_javascript_result_get_js_value" js-value-ref
  (js-result webkit-javascript-result))
(export 'webkit-javascript-result-get-js-value)

(defcfun ("webkit_web_view_send_message_to_page" %webkit-web-view-send-message-to-page) :void
  (view (g-object webkit-web-view))
  (message (g-object webkit-user-message))
  (cancellable :pointer)
  (callback g-async-ready-callback)
  (user-data :pointer))

(defcfun ("webkit_web_view_send_message_to_page_finish" %webkit-web-view-send-message-to-page-finish)
    (g-object webkit-user-message)
  (view (g-object webkit-web-view))
  (result g-async-result)
  (g-error :pointer))

(defun webkit-web-view-send-message-to-page-finish (view result)
  (glib:with-g-error (err)
    (%webkit-web-view-send-message-to-page-finish view result err)))
(export 'webkit-web-view-send-message-to-page-finish)

(cffi:defcallback message-replied-to
    :void ((source-object :pointer) (result :pointer) (user-data :pointer))
  (let ((callback (find (cffi:pointer-address user-data) callbacks :key (function callback-id))))
    (handler-case
        (let ((reply (webkit-web-view-send-message-to-page-finish source-object result)))
          (setf callbacks (delete callback callbacks))
          (when (callback-function callback)
            (funcall (callback-function callback) reply)))
      (condition (c)
        (when callback
          (when (callback-error-function callback)
            (funcall (callback-error-function callback) c))
          (setf callbacks (delete callback callbacks)))))))

(defun webkit-web-view-send-message-to-page (web-view message &optional call-back error-call-back)
  "Send MESSAGE to the WEB-VIEW corresponding page calling CALL-BACK upon completion and ERROR-CALL-BACK on error."
  (incf callback-counter)
  (push (make-callback :id callback-counter :web-view web-view
                       :function call-back
                       :error-function error-call-back)
        callbacks)
  (%webkit-web-view-send-message-to-page
   web-view message
   (cffi:null-pointer)
   (cffi:callback message-replied-to)
   (cffi:make-pointer callback-counter)))
(export 'webkit-web-view-send-message-to-page)

(defcfun "webkit_web_view_try_close" :void
  (view (g-object webkit-web-view)))
(export 'webkit-web-view-try-close)

#+webkit2-cors-allowlist
(defcfun ("webkit_web_view_set_cors_allowlist" %webkit-web-view-set-cors-allowlist) :void
  (view (g-object webkit-web-view))
  (allowlist (:pointer :string)))
#+webkit2-cors-allowlist
(defun webkit-web-view-set-cors-allowlist (view allowlist)
  "Set CORS allowlist for the VIEW to be ALLOWLIST.
ALLOWLIST should be a list of strings of a form \"[protocol]://[host]/[path]\"."
  (let ((ffi-allowlist (cffi:foreign-alloc :string :initial-contents allowlist
                                                   :count (length allowlist))))
    (%webkit-web-view-set-cors-allowlist view ffi-allowlist)))
#+webkit2-cors-allowlist
(export 'webkit-web-view-set-cors-allowlist)
