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

(in-package :webkit.foreign)

;;; ^L

(define-foreign-library libwebkit
  (:unix (:or "libwebkit-1.0.so")))

(use-foreign-library libwebkit)

;;; ^L

(defctype gtk-target-list :pointer)
(defctype gtk-movement-step :int)
(defctype gdk-event-button :pointer)

(defctype soup-session :pointer)

(defctype soup-message :pointer)

(defctype webkit-hit-test-result :pointer)

(defctype webkit-web-frame :pointer)

(defctype webkit-web-window-features :pointer)

(defctype webkit-web-inspector :pointer)

;; ^L

;; This is the central class of the WebKit API. It's a widget implementing the
;; scrolling interface.
;;
;; It is responsible for managing the drawing of content and forwarding of
;; events.
(defctype webkit-web-view :pointer)
(export 'webkit-web-view)

(defctype webkit-web-back-forward-list :pointer)
(export 'webkit-web-back-forward-list)

;; Control the behaviour of a webkit-web-view
;;
;; Properties:
;;
;; auto-load-images boolean
;; auto-resize-window boolean
;; auto-shrink-images boolean
;; cursive-font-family string
;; default-encoding string
;; default-font-family string
;; default-font-size int
;; default-monospace-font-size int
;; editing-behavior webkit-editing-behavior
;; enable-caret-browsing boolean
;; enable-default-context-menu boolean
;; enable-developer-extras boolean
;; enable-dom-paste boolean
;; enable-file-access-from-file-uris boolean
;; enable-html5-database boolean
;; enable-html5-local-storage boolean
;; enable-java-applet boolean
;; enable-offline-web-application-cache boolean
;; enable-page-cache boolean
;; enable-scripts boolean
;; enable-site-specific-quirks boolean
;; enable-spatial-navigation boolean
;; enable-spell-checking boolean
;; enable-universal-access-from-file-uris boolean
;; enable-xss-auditor boolean
;; enforce-96-dpi boolean
;; fantasy-font-family string
;; javascript-can-access-clipboard boolean
;; javascript-can-open-windows-automatically boolean
;; minimum-font-size int
;; minimum-logical-font-size int
;; monospace-font-family string
;; print-backgrounds boolean
;; resizable-text-areas boolean
;; sans-serif-font-family string
;; serif-font-family string
;; spell-checking-languages string
;; tab-key-cycles-through-elements boolean
;; user-agent string
;; user-stylesheet-uri string
;; zoom-step float
(defctype webkit-web-settings :pointer)
(export 'webkit-web-settings)

;; Represents the network related aspects of navigation request. Used whenever
;; WebKit wants to provide information about a request that will be sent, or has
;; been sent.
;;
;; For valid URIs, you also get a soup-message object, which provides access to
;; further information such as headers.
(defctype webkit-network-request :pointer)
(export 'webkit-network-request)

;; Represents the network related aspects of navigation response
(defctype webkit-network-response :pointer)
(export 'webkit-network-response)

;; Represents a downloaded URI, encapsulates the data of the download as well
;; as the URI, MIME type and frame name of the resource.
(defctype webkit-web-resource :pointer)
(export 'webkit-web-resource)

;; Used in signals to provide details about what led the navigation to happen.
(defctype webkit-web-navigation-action :pointer)
(export 'webkit-web-navigation-action)

;; A class that carries with it information about a download request.
;; Use this to control the download process, or to simply figure out what is
;; to be downlaoded, and do it yourself.
(defctype webkit-download :pointer)
(export 'webkit-download)

;; Liason between WebKit and the application regarding asynchronous policy decisions,
;; like opening new windows, redirection, etc.
;;
;; These objects are passed to the application on signal emissions that deal with
;; policy decision. They are used by the application to tell the engine what to do.
(defctype webkit-web-policy-decision :pointer)
(export 'webkit-web-policy-decision)

(defctype webkit-web-history-item :pointer)
(export 'webkit-web-history-item)

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

(defcfun "webkit_web_settings_new" webkit-web-settings)
(export 'webkit-web-settings-new)

(defcfun "webkit_web_settings_copy" webkit-web-settings
  (web-settings webkit-web-settings))
(export 'webkit-web-settings-copy)

(defcfun "webkit_web_settings_get_user_agent" :string ; *gchar
  (web-settings webkit-web-settings))
(export 'webkit-web-settings-get-user-agent)

;;; ^L
;;; webkitnetworkrequest.h

;; NOTE: returns NULL if the URI is invalid
(defcfun "webkit_network_request_new" webkit-network-request
  (uri :string))
(export 'webkit-network-request-new)

;; NOTE: if the request has an associated soup-message, its URI will also be set
;; by this call.
(defcfun "webkit_network_request_set_uri" :void
  (request webkit-network-request)
  (uri :string))
(export 'webkit-network-request-set-uri)

(defcfun "webkit_network_request_get_uri" :string
  (request webkit-network-request))
(export 'webkit-network-request-get-uri)

(defcfun "webkit_network_request_get_message" soup-message
  (request webkit-network-request))
(export 'webkit-network-request-get-message)

;;; ^L
;;; webkitnetworkresponse.h

(defcfun "webkit_network_response_new" webkit-network-response
  (uri :string))
(export 'webkit-network-response-new)

;; NOTE: when the response has an associated soup-message, its URI will also
;; be set by this call
(defcfun "webkit_network_response_set_uri" :void
  (response webkit-network-response)
  (uri :string))
(export 'webkit-network-response-set-uri)

(defcfun "webkit_network_response_get_uri" :string
  (response webkit-network-response))
(export 'webkit-network-response-get-uri)

(defcfun "webkit_network_response_get_message" soup-message
  (response webkit-network-response))
(export 'webkit-network-response-get-message)

;;; ^L
;;; webkitwebresource.h

;; NOTE: encoding can be NULL
;; NOTE: frame_name can be used if the resource represents contents of an entire HTML
;; frame, otherwise pass NULL
(defcfun "webkit_web_resource_new" webkit-web-resource
  (data :string)
  (size :int) ; is really `gssize'
  (uri :string)
  (mime-type :string)
  (encoding :string)
  (frame-name :string))
(export 'webkit-web-resource-new)

;; NOTE: the web-resource is owned by WebKit and should not be freed
(defcfun "webkit_web_resource_get_data" glib:g-string ; this is GString
  (web-resource webkit-web-resource))
(export 'webkit-web-resource-get-data)

(defcfun "webkit_web_resource_get_uri" :string
  (web-resource webkit-web-resource))
(export 'webkit-web-resource-get-uri)

(defcfun "webkit_web_resource_get_mime_type" :string
  (web-resource webkit-web-resource))
(export 'webkit-web-resource-get-mime-type)

(defcfun "webkit_web_resource_get_encoding" :string
  (web-resource webkit-web-resource))
(export 'webkit-web-resource-get-encoding)

(defcfun "webkit_web_resource_get_frame_name" :string
  (web-resource webkit-web-resource))
(export 'webkit-web-resource-get-frame-name)

;;; ^L
;;; webkitwebnavigationaction.h

;; The reason why WebKit is requesting a navigation.
(defcfun "webkit_web_navigation_action_get_reason" webkit-web-navigation-reason
  (navigation-action webkit-web-navigation-action))
(export 'webkit-web-navigation-action-get-reason)

(defcfun "webkit_web_navigation_action_set_reason" :void
  (navigation-action webkit-web-navigation-action)
  (reason webkit-web-navigation-reason))
(export 'webkit-web-navigation-action-set-reason)

;; The URI that was originally requested. May differ from navigation target (due to redirects).
(defcfun "webkit_web_navigation_action_get_original_uri" :string
  (navigation-action webkit-web-navigation-action))
(export 'webkit-web-navigation-action-get-original-uri)

(defcfun "webkit_web_navigation_action_set_original_uri" :void
  (navigation-action webkit-web-navigation-action)
  (uri :string))
(export 'webkit-web-navigation-action-set-original-uri)

;; The DOM identifier for the mouse button used to click.
;; Values are 0, 1 and 2 for left, middle and right buttons.
;; Actions not initiated by a mouse click are denoted by -1.
(defcfun "webkit_web_navigation_action_get_button" :int
  (navigation-action webkit-web-navigation-action))
(export 'webkit-web-navigation-action-get-button)

;; The bitmask with the state of the modifier keys.
(defcfun "webkit_web_navigation_action_get_modifier_state" :int
  (navigation-action webkit-web-navigation-action))
(export 'webkit-web-navigation-action-get-modifier-state)

(defcfun "webkit_web_navigation_action_get_target_frame" :string
  (navigation-action webkit-web-navigation-action))
(export 'webkit-web-navigation-action-get-target-frame)

;;; ^L
;;; webkitdownload.h
;;;

(defcfun "webkit_download_new" webkit-download
  (request webkit-network-request))
(export 'webkit-download-new)

;; Initiate the download.
(defcfun "webkit_download_start" :void
  (download webkit-download))
(export 'webkit-download-start)

;; Cancel the download.
;;
;; TODO: this will not free the download object, it must be freed with `g_object_unref()'
;; NOTE: this emits a WebKitDownload::error signal
(defcfun "webkit_download_cancel" :void
  (download webkit-download))
(export 'webkit-download-cancel)

(defcfun "webkit_download_get_uri" :string
  (download webkit-download))
(export 'webkit-download-get-uri)

(defcfun "webkit_download_get_network_request" webkit-network-request
  (download webkit-download))
(export 'webkit-download-get-network-request)

(defcfun "webkit_download_get_network_response" webkit-network-response
  (download webkit-download))
(export 'webkit-download-get-network-response)

;; The filename that was suggested by the server, or the one derived by WebKit from the URI.
(defcfun "webkit_download_get_suggested_filename" :string
  (download webkit-download))
(export 'webkit-download-get-suggested-filename)

(defcfun "webkit_download_get_destination_uri" :string
  (download webkit-download))
(export 'webkit-download-get-destination-uri)

;; Obtain the URI to which the downloaded file will be written.
;;
;; NOTE: must be set before calling `webkit_download_start()'
;; NOTE: may be NULL
(defcfun "webkit_download_set_destination_uri" :void
  (download webkit-download)
  (destination-uri :string))
(export 'webkit-download-set-destination-uri)

;; Determine the current progress of the download. Returns a number ranging
;; from 0.0 to 1.0
(defcfun "webkit_download_get_progress" :double
  (download webkit-download))
(export 'webkit-download-get-progress)

;; Returns the number of seconds since the download was started.
(defcfun "webkit_download_get_elapsed_time" :double
  (download webkit-download))
(export 'webkit-download-get-elapsed-time)

;; The expected total size of the download. May be incorrect.
(defcfun "webkit_download_get_total_size" :uint64
  (download webkit-download))
(export 'webkit-download-get-total-size)

(defcfun "webkit_download_get_status" webkit-download-status
  (download webkit-download))

;;; ^L
;;; webkitwebpolicydecision.h

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

(defcfun "webkit_web_history_item_new" webkit-web-history-item)
(export 'webkit-web-history-item-new)

(defcfun "webkit_web_history_item_new_with_data" webkit-web-history-item
  (uri :string)
  (title :string))
(export 'webkit-web-history-item-new-with-data)

(defcfun "webkit_web_history_item_get_title" :string
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-get-title)

(defcfun "webkit_web_history_item_get_alternate_title" :string
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-get-alternate-title)

(defcfun "webkit_web_history_item_set_alternate_title" :void
  (web-history-item webkit-web-history-item)
  (title :string))
(export 'webkit-web-history-item-set-alternate-title)

(defcfun "webkit_web_history_item_get_uri" :string
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-get-uri)

(defcfun "webkit_web_history_item_get_original_uri" :string
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-get-original-uri)

(defcfun "webkit_web_history_item_get_last_visited_time" :double
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-get-last-visited-time)

(defcfun "webkit_web_history_item_copy" webkit-web-history-item
  (web-history-item webkit-web-history-item))
(export 'webkit-web-history-item-copy)

;;; ^L
;;; webkitwebbackforwardlist.h

(defcfun "webkit_web_back_forward_list_new_with_web_view" webkit-web-back-forward-list
  (web-view webkit-web-view))
(export 'webkit-web-back-forward-list-new-with-web-view)

(defcfun "webkit_web_back_forward_list_go_forward" :void
  (web-back-forward-list webkit-web-back-forward-list))
(export 'webkit-web-back-forward-list-go-forward)

(defcfun "webkit_web_back_forward_list_go_back" :void
  (web-back-forward-list webkit-web-back-forward-list))
(export 'webkit-web-back-forward-list-go-back)

(defcfun "webkit_web_back_forward_list_contains_item" :boolean
  (web-back-forward-list webkit-web-back-forward-list)
  (history-item webkit-web-history-item))
(export 'webkit-web-back-forward-list-contains-item)

(defcfun "webkit_web_back_forward_list_go_to_item" :void
  (web-back-forward-list webkit-web-back-forward-list)
  (history-item webkit-web-history-item))
(export 'webkit-web-back-forward-list-go-to-item)

;; XXX: why do we get errors with glib:glist as the return type?
(defcfun "webkit_web_back_forward_list_get_forward_list_with_limit" :pointer ; GList
  (web-back-forward-list webkit-web-back-forward-list)
  (limit :int))
(export 'webkit-web-back-forward-list-get-forward-list-with-limit)

;; XXX: why do we get errors with glib:glist as the return type?
(defcfun "webkit_web_back_forward_list_get_back_list_with_limit" :pointer ; GList
  (web-back-forward-list webkit-web-back-forward-list)
  (limit :int))
(export 'webkit-web-back-forward-list-get-back-list-with-limit)

(defcfun "webkit_web_back_forward_list_get_back_item" webkit-web-history-item
  (web-back-forward-list webkit-web-back-forward-list))
(export 'webkit-web-back-forward-list-get-back-item)

(defcfun "webkit_web_back_forward_list_get_current_item" webkit-web-history-item
  (web-back-forward-list webkit-web-back-forward-list))
(export 'webkit-web-back-forward-list-get-current-item)

(defcfun "webkit_web_back_forward_list_get_forward_item" webkit-web-history-item
  (web-back-forward-list webkit-web-back-forward-list))
(export 'webkit-web-back-forward-list-get-forward-item)

(defcfun "webkit_web_back_forward_list_get_nth_item" webkit-web-history-item
  (web-back-forward-list webkit-web-back-forward-list)
  (index :int))
(export 'webkit-web-back-forward-list-get-nth-item)

(defcfun "webkit_web_back_forward_list_get_back_length" :int
  (web-back-forward-list webkit-web-back-forward-list))
(export 'webkit-web-back-forward-list-get-back-length)

(defcfun "webkit_web_back_forward_list_get_forward_length" :int
  (web-back-forward-list webkit-web-back-forward-list))
(export 'webkit-web-back-forward-list-get-forward-length)

(defcfun "webkit_web_back_forward_list_get_limit" :int
  (web-back-forward-list webkit-web-back-forward-list))
(export 'webkit-web-back-forward-list-get-limit)

(defcfun "webkit_web_back_forward_list_set_limit" :void
  (web-back-forward-list webkit-web-back-forward-list)
  (limit :int))
(export 'webkit-web-back-forward-list-set-limit)

(defcfun "webkit_web_back_forward_list_add_item" :void
  (web-back-forward-list webkit-web-back-forward-list)
  (history-item webkit-web-history-item))
(export 'webkit-web-back-forward-list-add-item)

(defcfun "webkit_web_back_forward_list_clear" :void
  (web-back-forward-list webkit-web-back-forward-list))
(export 'webkit-web-back-forward-list-clear)

;;; ^L
;;; webkitwebview.h

;; TODO: this needs to be initialized!
(defcfun "webkit_web_view_new" webkit-web-view)
(export 'webkit-web-view-new)

(defcfun "webkit_web_view_get_title" :string
  (web-view webkit-web-view))
(export 'webkit-web-view-get-title)

(defcfun "webkit_web_view_get_uri" :string
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
  (uri :string))
(export 'webkit-web-view-open)

(defcfun "webkit_web_view_reload" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-reload)

(defcfun "webkit_web_view_reload_bypass_cache" :void
  (web-view webkit-web-view))
(export 'webkit-web-view-reload-bypass-cache)

(defcfun ("webkit_web_view_load_uri" %webkit-web-view-load-uri) :void
  (web-view webkit-web-view)
  (uri :string))

(defun webkit-web-view-load-uri (view uri)
  (with-foreign-string (c-uri uri)
    (%webkit-web-view-load-uri view c-uri)))
(export 'webkit-web-view-load-uri)

(defcfun "webkit_web_view_load_string" :void
  (web-view webkit-web-view)
  (content :string)
  (mime-type :string)
  (encoding :string)
  (base-uri :string))
(export 'webkit-web-view-load-string)

(defcfun "webkit_web_view_load_html_string" :void
  (web-view webkit-web-view)
  (content :string)
  (base-uri :string))
(export 'webkit-web-view-load-html-string)

(defcfun "webkit_web_view_load_request" :void
  (web-view webkit-web-view)
  (request webkit-network-request))
(export 'webkit-web-view-load-request)

(defcfun "webkit_web_view_search_text" :boolean
  (web-view webkit-web-view)
  (text :string)
  (case-sensitive :boolean)
  (forward :boolean)
  (wrap :boolean))
(export 'webkit-web-view-search-text)

(defcfun "webkit_web_view_mark_text_matches" :uint
  (web-view webkit-web-view)
  (string :string)
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
  (script :string))
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
  (mime-type :string))
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

(defcfun "webkit_web_view_get_encoding" :string
  (web-view webkit-web-view))
(export 'webkit-web-view-get-encoding)

(defcfun "webkit_web_view_set_custom_encoding" :void
  (web-view webkit-web-view)
  (encoding :string))
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

(defcfun "webkit_web_view_get_icon_uri" :string
  (web-view webkit-web-view))
(export 'webkit-web-view-get-icon-uri)

(defcfun "webkit_set_cache_model" :void
  (cache-model webkit-cache-model))
(export 'webkit-set-cache-model)

(defcfun "webkit_get_cache_model" webkit-cache-model)
(export 'webkit-get-cache-model)
