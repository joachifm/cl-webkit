;; Copyright (C) 2010, Joachim Fasting
;;
;;   This file is part of cl-webkit
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistribution of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistribution in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Commentary:
;;
;; Low-level bindings
;;
;; Code:

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(in-package :webkit)

;;; ^L

(define-foreign-library libwebkit
  (:unix (:or "libwebkit-1.0.so")))

(use-foreign-library libwebkit)

;;; ^L

(defctype g-type :pointer)
(defctype gtk-target-list :pointer)
(defctype gtk-movement-step :int)
(defctype gdk-event-button :pointer)

(defctype soup-session :pointer)

(defctype soup-message :pointer)

(defctype webkit-hit-test-result :pointer)

(defctype webkit-web-frame :pointer)

(defctype webkit-web-window-features :pointer)

(defctype webkit-web-inspector :pointer)

(defctype webkit-web-back-forward-list :pointer)

;;; ^L
;;; webkitversion.h

(defcfun "webkit_major_version" :uint)
(export 'webkit-major-version)

(defcfun "webkit_minor_version" :uint)
(export 'webkit-minor-version)

(defcfun "webkit_micro_version" :uint)
(export 'webkit-micro-version)

;;; ^L
;;; webkitwebsettings.h

(defctype webkit-web-settings :pointer)
(export 'webkit-web-settings)

(defcfun "webkit_web_settings_new" webkit-web-settings)
(export 'webkit-web-settings-new)

(defcfun "webkit_web_settings_copy" webkit-web-settings
  (web-settings webkit-web-settings))
(export 'webkit-web-settings-copy)

(defcfun "webkit_web_settings_get_user_agent" glib:g-string
  (web-settings webkit-web-settings))
(export 'webkit-web-settings-get-user-agent)

;;; ^L
;;; webkitnetworkrequest.h

(defctype webkit-network-request :pointer)
(export 'webkit-network-request)

(defcfun "webkit_network_request_new" webkit-network-request
  (uri glib:g-string))
(export 'webkit-network-request-new)

(defcfun "webkit_network_request_set_uri" :void
  (request webkit-network-request)
  (uri glib:g-string))
(export 'webkit-network-request-set-uri)

(defcfun "webkit_network_request_get_uri" glib:g-string
  (request webkit-network-request))
(export 'webkit-network-request-get-uri)

(defcfun "webkit_network_request_get_message" soup-message
  (request webkit-network-request))
(export 'webkit-network-request-get-message)

;;; ^L
;;; webkitnetworkresponse.h

(defctype webkit-network-response :pointer)
(export 'webkit-network-response)

(defcfun "webkit_network_response_new" webkit-network-response
  (uri glib:g-string))
(export 'webkit-network-response-new)

(defcfun "webkit_network_response_set_uri" :void
  (response webkit-network-response)
  (uri glib:g-string))
(export 'webkit-network-response-set-uri)

(defcfun "webkit_network_response_get_uri" glib:g-string
  (response webkit-network-response))
(export 'webkit-network-response-get-uri)

(defcfun "webkit_network_response_get_message" soup-message
  (response webkit-network-response))
(export 'webkit-network-response-get-message)

;;; ^L
;;; webkitwebresource.h

(defctype webkit-web-resource :pointer)
(export 'webkit-web-resource)

(defcfun "webkit_web_resource_new" webkit-web-resource
  (data glib:g-string)
  (size :int) ; is really `gssize'
  (uri glib:g-string)
  (mime-type glib:g-string)
  (encoding glib:g-string)
  (frame-name glib:g-string))
(export 'webkit-web-resource-new)

(defcfun "webkit_web_resource_get_data" glib:g-string ; this is GString
  (web-resource webkit-web-resource))
(export 'webkit-web-resource-get-data)

(defcfun "webkit_web_resource_get_uri" glib:g-string
  (web-resource webkit-web-resource))
(export 'webkit-web-resource-get-uri)

(defcfun "webkit_web_resource_get_mime_type" glib:g-string
  (web-resource webkit-web-resource))
(export 'webkit-web-resource-get-mime-type)

(defcfun "webkit_web_resource_get_encoding" glib:g-string
  (web-resource webkit-web-resource))
(export 'webkit-web-resource-get-encoding)

(defcfun "webkit_web_resource_get_frame_name" glib:g-string
  (web-resource webkit-web-resource))
(export 'webkit-web-resource-get-frame-name)

;;; ^L
;;; webkitwebnavigationaction.h

(defctype webkit-web-navigation-action :pointer)
(export 'webkit-web-navigation-action)

(defcfun "webkit_web_navigation_action_get_reason" webkit-web-navigation-reason
  (navigation-action webkit-web-navigation-action))
(export 'webkit-web-navigation-action-get-reason)

(defcfun "webkit_web_navigation_action_set_reason" :void
  (navigation-action webkit-web-navigation-action)
  (reason webkit-web-navigation-reason))
(export 'webkit-web-navigation-action-set-reason)

(defcfun "webkit_web_navigation_action_get_original_uri" glib:g-string
  (navigation-action webkit-web-navigation-action))
(export 'webkit-web-navigation-action-get-original-uri)

(defcfun "webkit_web_navigation_action_set_original_uri" :void
  (navigation-action webkit-web-navigation-action)
  (uri glib:g-string))
(export 'webkit-web-navigation-action-set-original-uri)

(defcfun "webkit_web_navigation_action_get_button" :int
  (navigation-action webkit-web-navigation-action))
(export 'webkit-web-navigation-action-get-button)

(defcfun "webkit_web_navigation_action_get_modifier_state" :int
  (navigation-action webkit-web-navigation-action))
(export 'webkit-web-navigation-action-get-modifier-state)

(defcfun "webkit_web_navigation_action_get_target_frame" glib:g-string
  (navigation-action webkit-web-navigation-action))
(export 'webkit-web-navigation-action-get-target-frame)

;;; ^L
;;; webkitdownload.h

(defctype webkit-download :pointer)
(export 'webkit-download)

(defcfun "webkit_download_new" webkit-download
  (request webkit-network-request))
(export 'webkit-download-new)

(defcfun "webkit_download_start" :void
  (download webkit-download))
(export 'webkit-download-start)

(defcfun "webkit_download_cancel" :void
  (download webkit-download))
(export 'webkit-download-cancel)

(defcfun "webkit_download_get_uri" glib:g-string
  (download webkit-download))
(export 'webkit-download-get-uri)

(defcfun "webkit_download_get_network_request" webkit-network-request
  (download webkit-download))
(export 'webkit-download-get-network-request)

(defcfun "webkit_download_get_network_response" webkit-network-response
  (download webkit-download))
(export 'webkit-download-get-network-response)

(defcfun "webkit_download_get_suggested_filename" glib:g-string
  (download webkit-download))
(export 'webkit-download-get-suggested-filename)

(defcfun "webkit_download_get_destination_uri" glib:g-string
  (download webkit-download))
(export 'webkit-download-get-destination-uri)

(defcfun "webkit_download_set_destination_uri" :void
  (download webkit-download)
  (destination-uri glib:g-string))
(export 'webkit-download-set-destination-uri)

(defcfun "webkit_download_get_progress" :double
  (download webkit-download))
(export 'webkit-download-get-progress)

(defcfun "webkit_download_get_elapsed_time" :double
  (download webkit-download))
(export 'webkit-download-get-elapsed-time)

(defcfun "webkit_download_get_total_size" :uint64
  (download webkit-download))
(export 'webkit-download-get-total-size)

(defcfun "webkit_download_get_status" webkit-download-status
  (download webkit-download))

;;; ^L
;;; webkitwebpolicydecision.h

(defctype webkit-web-policy-decision :pointer)
(export 'webkit-web-policy-decision)

(defcfun "webkit_web_policy_decision_use" :void
  (decision webkit-web-policy-decision))
(export 'webkit-web-policy-decision-use)

(defcfun "webkit_web_policy_decision_ignore" :void
  (decision webkit-web-policy-decision))
(export 'webkit-web-policy-decision-ignore)

(defcfun "webkit_web_policy_decision_download" :void
  (decision webkit-web-policy-decision))
(export 'webkit-web-policy-decision-download)

;;; ^L
;;; webkitwebhistoryitem.h

(defctype webkit-web-history-item :pointer)
(export 'webkit-web-history-item)

(defcfun "webkit_web_history_item_new" webkit-web-history-item)
(export 'webkit-web-history-item-new)

(defcfun "webkit_web_history_item_new_with_data" webkit-web-history-item
  (uri glib:g-string)
  (title glib:g-string))
(export 'webkit-web-history-item-new-with-data)

(defcfun "webkit_web_history_item_get_title" glib:g-string
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-get-title)

(defcfun "webkit_web_history_item_get_alternate_title" glib:g-string
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-get-alternate-title)

(defcfun "webkit_web_history_item_set_alternate_title" :void
  (web-history-item webkit-web-history-item)
  (title glib:g-string))
(export 'webkit-web-history-item-set-alternate-title)

(defcfun "webkit_web_history_item_get_uri" glib:g-string
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-get-uri)

(defcfun "webkit_web_history_item_get_original_uri" glib:g-string
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-get-original-uri)

(defcfun "webkit_web_history_item_get_last_visited_time" :double
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-get-last-visited-time)

(defcfun "webkit_web_history_item_copy" webkit-web-history-item
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-copy)

;;; ^L
;;; webkitwebview.h

;; This is the central class of the WebKit API. It's a widget implementing the
;; scrolling interface.
;;
;; It is responsible for managing the drawing of content and forwarding of
;; events.
;;
;; Each WebkitWebView has exactly one WebKitWebFrame as main frame.
(defctype webkit-web-view :pointer)
(export 'webkit-web-view)

;; TODO: this needs to be initialized!
(defcfun "webkit_web_view_new" webkit-web-view)
(export 'webkit-web-view-new)

(defcfun "webkit_web_view_get_title" glib:g-string
  (web-view webkit-web-view))
(export 'webkit-web-view-get-title)

(defcfun "webkit_web_view_get_uri" glib:g-string
  (web-view webkit-web-view))
(export 'webkit-web-view-get-uri)

(defcfun "webkit_web_view_set_maintains_back_forward_list" :void
  (web-view webkit-web-view)
  (mode :boolean))
(export 'webkit-web-view-set-maintains-back-forward-list)

(defcfun "webkit_web_view_get_back_forward_list" webkit-web-back-forward-list
  (web-view webkit-web-view))
(export 'wekbit-web-view-get-back-forward-list)

(defcfun "webkit_web_view_go_to_back_forward_item" :boolean
  (web-view webkit-web-view)
  (item webkit-web-history-item))
(export 'webkit-web-view-go-to-back-forward-item)

(defcfun "webkit_web_view_can_go_back" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-can-go-back)

(defcfun "webkit_web_view_can_go_back_or_forward" :boolean
  (web-view webkit-web-view)
  (steps :int)) ;; Negative means backward, positive forward
(export 'webkit-web-view-can-go-back-or-forward)

(defcfun "webkit_web_view_can_go_forward" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-can-go-forward)

(defcfun "webkit_web_view_go_forward" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-go-forward)

(defcfun "webkit_web_view_stop_loading" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-stop-loading)

(defcfun "webkit_web_view_open" :void
  (web-view webkit-web-view)
  (uri glib:g-string))
(export 'webkit-web-view-open)

(defcfun "webkit_web_view_reload" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-reload)

(defcfun "webkit_web_view_reload_bypass_cache" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-reload-bypass-cache)

(defcfun ("webkit_web_view_load_uri" %webkit-web-view-load-uri) :void
  (web-view webkit-web-view)
  (uri glib:g-string))

(defun webkit-web-view-load-uri (view uri)
  (with-foreign-string (c-uri uri)
    (%webkit-web-view-load-uri view c-uri)))
(export 'webkit-web-view-load-uri)

(defcfun "webkit_web_view_load_string" :void
  (web-view webkit-web-view)
  (content glib:g-string)
  (mime-type glib:g-string)
  (encoding glib:g-string)
  (base-uri glib:g-string))
(export 'webkit-web-view-load-string)

(defcfun "webkit_web_view_load_html_string" :void
  (web-view webkit-web-view)
  (content glib:g-string)
  (base-uri glib:g-string))
(export 'webkit-web-view-load-html-string)

(defcfun "webkit_web_view_load_request" :void
  (web-view webkit-web-view)
  (request webkit-network-request))
(export 'webkit-web-view-load-request)

(defcfun "webkit_web_view_search_text" :boolean
  (web-view webkit-web-view)
  (text glib:g-string)
  (case-sensitive :boolean)
  (forward :boolean)
  (wrap :boolean))
(export 'webkit-web-view-search-text)

(defcfun "webkit_web_view_mark_text_matches" :uint
  (web-view webkit-web-view)
  (string glib:g-string)
  (case-sensitive :boolean)
  (limit :uint))
(export 'webkit-web-view-mark-text-matches)

(defcfun "webkit_web_view_set_highlight_text_matches" :void
  (web-view webkit-web-view)
  (highlight :boolean))
(export 'webkit-web-view-set-highlight-text-matches)

(defcfun "webkit_web_view_unmark_text_matches" :void
  (web-view webkit-web-view))
(export 'wekbit-web-view-unmark-text-matches)

(defcfun "webkit_web_view_get_main_frame" webkit-web-frame
  (web-view webkit-web-view))
(export 'webkit-web-view-get-main-frame)

(defcfun "webkit_web_view_get_focused_frame" webkit-web-frame
  (web-view webkit-web-view))
(export 'webkit-web-view-get-focused-frame)

(defcfun "webkit_web_view_execute_script" :void
  (web-view webkit-web-view)
  (script glib:g-string))
(export 'webkit-web-view-execute-script)

(defcfun "webkit_web_view_can_cut_clipboard" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-can-cut-clipboard)

(defcfun "webkit_web_view_can_copy_clipboard" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-can-copy-clipboard)

(defcfun "webkit_web_view_can_paste_clipboard" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-can-paste-clipboard)

(defcfun "webkit_web_view_cut_clipboard" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-cut-clipboard)

(defcfun "webkit_web_view_copy_clipboard" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-copy-clipboard)

(defcfun "webkit_web_view_paste_clipboard" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-paste-clipboard)

(defcfun "webkit_web_view_delete_selection" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-delete-selection)

(defcfun "webkit_web_view_has_selection" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-has-selection)

(defcfun "webkit_web_view_select_all" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-select-all)

(defcfun "webkit_web_view_get_editable" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-get-editable)

(defcfun "webkit_web_view_set_editable" :void
  (web-view webkit-web-view)
  (mode :boolean))
(export 'webkit-web-view-set-editable)

(defcfun "webkit_web_view_get_copy_target_list" gtk-target-list
  (web-view webkit-web-view))
(export 'webkit-web-view-get-copy-target-list)

(defcfun "webkit_web_view_get_paste_target_list" gtk-target-list
  (web-view webkit-web-view))
(export 'webkit-web-view-get-paste-target-list)

(defcfun "webkit_web_view_set_settings" :void
  (web-view webkit-web-view)
  (settings webkit-web-settings))
(export 'webkit-web-view-set-settings)

(defcfun "webkit_web_view_get_settings" webkit-web-settings
  (web-view webkit-web-view))
(export 'webkit-web-view-get-settings)

(defcfun "webkit_web_view_get_inspector" webkit-web-inspector
  (web-view webkit-web-view))
(export 'webkit-web-view-get-inspector)

(defcfun "webkit_web_view_get_window_features" webkit-web-window-features
  (web-view webkit-web-view))
(export 'webkit-web-view-get-window-features)

(defcfun "webkit_web_view_can_show_mime_type" :boolean
  (web-view webkit-web-view)
  (mime-type glib:g-string))
(export 'webkit-web-view-can-show-mime-type)

(defcfun "webkit_web_view_get_transparent" :void
  (web-view webkit-web-view)
  (mode :boolean))
(export 'webkit-web-view-get-transparent)

(defcfun "webkit_web_view_get_zoom_level" :float
  (web-view webkit-web-view))
(export 'webkit-web-view-get-zoom-level)

(defcfun "webkit_web_view_set_zoom_level" :void
  (web-view webkit-web-view)
  (level :float))
(export 'webkit-web-view-set-zoom-level)

(defcfun "webkit_web_view_zoom_in" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-zoom-in)

(defcfun "webkit_web_view_zoom_out" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-zoom-out)

(defcfun "webkit_web_view_get_full_content_zoom" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-get-full-content-zoom)

(defcfun "webkit_web_view_set_full_content_zoom" :void
  (web-view webkit-web-view)
  (full-content-zoom :boolean))
(export 'webkit-web-view-set-full-content-zoom)

(defcfun "webkit_get_default_session" soup-session)
(export 'webkit-get-default-session)

(defcfun "webkit_web_view_get_encoding" glib:g-string
  (web-view webkit-web-view))
(export 'webkit-web-view-get-encoding)

(defcfun "webkit_web_view_set_custom_encoding" :void
  (web-view webkit-web-view)
  (encoding glib:g-string))
(export 'webkit-web-view-set-custom-encoding)

(defcfun "webkit_web_view_move_cursor" :void
  (web-view webkit-web-view)
  (step gtk-movement-step)
  (count :int))
(export 'webkit-web-view-move-cursor)

(defcfun "webkit_web_view_get_load_status" webkit-load-status
  (web-view webkit-web-view))
(export 'webkit-web-view-get-load-status)

(defcfun "webkit_web_view_get_progress" :double
  (web-view webkit-web-view))
(export 'webkit-web-view-get-progress)

(defcfun "webkit_web_view_undo" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-undo)

(defcfun "webkit_web_view_can_undo" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-can-undo)

(defcfun "webkit_web_view_redo" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-redo)

(defcfun "webkit_web_view_can_redo" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-can-redo)

(defcfun "webkit_web_view_set_view_source_mode" :void
  (web-view webkit-web-view)
  (view-source-mode :boolean))
(export 'webkit-web-view-set-view-source-mode)

(defcfun "webkit_web_view_get_view_source_mode" :boolean
  (web-view webkit-web-view))
(export 'webkit-web-view-get-view-source-mode)

(defcfun "webkit_web_view_get_hit_test_result" webkit-hit-test-result
  (web-view webkit-web-view)
  (event gdk-event-button))
(export 'webkit-web-view-get-hit-test-result)

(defcfun "webkit_web_view_get_icon_uri" glib:g-string
  (web-view webkit-web-view))
(export 'webkit-web-view-get-icon-uri)

(defcfun "webkit_set_cache_model" :void
  (cache-model webkit-cache-model))
(export 'webkit-set-cache-model)

(defcfun "webkit_get_cache_model" webkit-cache-model)
(export 'webkit-get-cache-model)
