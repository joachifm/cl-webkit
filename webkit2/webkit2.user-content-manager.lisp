;;; webkit2.user-content-manager.lisp --- bindings for WebKitUserContentManager

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitUserContentManager" () ())

(defcfun "webkit_user_content_manager_new" (g-object webkit-user-content-manager))
(export 'webkit-user-content-manager-new)

(defcfun "webkit_user_content_manager_add_style_sheet" :void
  (manager (g-object webkit-user-content-manager))
  (stylesheet webkit-user-style-sheet))
(export 'webkit-user-content-manager-add-style-sheet)

(defcfun "webkit_user_content_manager_remove_all_style_sheets" :void
  (manager (g-object webkit-user-content-manager)))
(export 'webkit-user-content-manager-remove-all-style-sheets)

(defcfun "webkit_user_content_manager_add_script" :void
  (manager (g-object webkit-user-content-manager))
  (script webkit-user-script))
(export 'webkit-user-content-manager-add-script)

(defcfun "webkit_user_content_manager_remove_all_scripts" :void
  (manager (g-object webkit-user-content-manager)))
(export 'webkit-user-content-manager-remove-all-scripts)

(defcfun "webkit_user_content_manager_register_script_message_handle" :boolean
  (manager (g-object webkit-user-content-manager))
  (name :string))
(export 'webkit-user-content-manager-register-script-message-handle)

(defcfun "webkit_user_content_manager_unregister_script_message_handler" :void
  (manager (g-object webkit-user-content-manager))
  (name :string))
(export 'webkit-user-content-manager-unregister-script-message-handler)

(defcfun "webkit_user_content_manager_register_script_message_handler_in_world" :boolean
  (manager (g-object webkit-user-content-manager))
  (name :string)
  (world-name :string))
(export 'webkit-user-content-manager-register-script-message-handler-in-world)

(defcfun "webkit_user_content_manager_unregister_script_message_handler_in_world" :void
  (manager (g-object webkit-user-content-manager))
  (name :string)
  (world-name :string))
(export 'webkit-user-content-manager-unregister-script-message-handler-in-world)

(defcfun "webkit_user_content_manager_add_filter" :void
  (manager (g-object webkit-user-content-manager))
  (filter webkit-user-content-filter))
(export 'webkit-user-content-manager-add-filter)

(defcfun "webkit_user_content_manager_remove_filter" :void
  (manager (g-object webkit-user-content-manager))
  (filter webkit-user-content-filter))
(export 'webkit-user-content-manager-remove-filter)

(defcfun "webkit_user_content_manager_remove_filter_by_id" :void
  (manager (g-object webkit-user-content-manager))
  (filter-id :string))
(export 'webkit-user-content-manager-remove-filter-by-id)

(defcfun "webkit_user_content_manager_remove_all_filters" :void
  (manager (g-object webkit-user-content-manager)))
(export 'webkit-user-content-manager-remove-all-filters)

