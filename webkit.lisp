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

(defctype webkit-hit-test-result :pointer)

(defctype webkit-web-frame :pointer)

(defctype webkit-web-window-features :pointer)

(defctype webkit-web-inspector :pointer)

(defctype webkit-web-settings :pointer)

(defctype webkit-web-back-forward-list :pointer)

(defctype webkit-web-history-item :pointer)

(defctype webkit-network-request :pointer)

;;; ^L
;;; webkitversion.h

(defcfun "webkit_major_version" :uint)
(export 'webkit-major-version)

(defcfun "webkit_minor_version" :uint)
(export 'webkit-minor-version)

(defcfun "webkit_micro_version" :uint)
(export 'webkit-micro-version)

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
