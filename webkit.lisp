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
(defcfun "webkit_minor_version" :uint)
(defcfun "webkit_micro_version" :uint)

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

(defcfun "webkit_web_view_get_type" g-type)

;; TODO: this needs to be initialized!
(defcfun "webkit_web_view_new" webkit-web-view)

(defcfun "webkit_web_view_get_title" glib:g-string
  (web-view webkit-web-view))

(defcfun "webkit_web_view_get_uri" glib:g-string
  (web-view webkit-web-view))

(defcfun "webkit_web_view_set_maintains_back_forward_list" :void
  (web-view webkit-web-view)
  (mode :boolean))

(defcfun "webkit_web_view_get_back_forward_list" webkit-web-back-forward-list
  (web-view webkit-web-view))

(defcfun "webkit_web_view_go_to_back_forward_item" :boolean
  (web-view webkit-web-view)
  (item webkit-web-history-item))

(defcfun "webkit_web_view_can_go_back" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_can_go_back_or_forward" :boolean
  (web-view webkit-web-view)
  (steps :int)) ;; Negative means backward, positive forward

(defcfun "webkit_web_view_can_go_forward" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_go_forward" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_stop_loading" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_open" :void
  (web-view webkit-web-view)
  (uri glib:g-string))

(defcfun "webkit_web_view_reload" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_reload_bypass_cache" :void
  (web-view webkit-web-view))

(defcfun ("webkit_web_view_load_uri" %webkit-web-view-load-uri) :void
  (web-view webkit-web-view)
  (uri glib:g-string))

(defun webkit-web-view-load-uri (view uri)
  (with-foreign-string (c-uri uri)
    (%webkit-web-view-load-uri view c-uri)))

(defcfun "webkit_web_view_load_string" :void
  (web-view webkit-web-view)
  (content glib:g-string)
  (mime-type glib:g-string)
  (encoding glib:g-string)
  (base-uri glib:g-string))

(defcfun "webkit_web_view_load_html_string" :void
  (web-view webkit-web-view)
  (content glib:g-string)
  (base-uri glib:g-string))

(defcfun "webkit_web_view_load_request" :void
  (web-view webkit-web-view)
  (request webkit-network-request))

(defcfun "webkit_web_view_search_text" :boolean
  (web-view webkit-web-view)
  (text glib:g-string)
  (case-sensitive :boolean)
  (forward :boolean)
  (wrap :boolean))

(defcfun "webkit_web_view_mark_text_matches" :uint
  (web-view webkit-web-view)
  (string glib:g-string)
  (case-sensitive :boolean)
  (limit :uint))

(defcfun "webkit_web_view_set_highlight_text_matches" :void
  (web-view webkit-web-view)
  (highlight :boolean))

(defcfun "webkit_web_view_unmark_text_matches" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_get_main_frame" webkit-web-frame
  (web-view webkit-web-view))

(defcfun "webkit_web_view_get_focused_frame" webkit-web-frame
  (web-view webkit-web-view))

(defcfun "webkit_web_view_execute_script" :void
  (web-view webkit-web-view)
  (script glib:g-string))

(defcfun "webkit_web_view_can_cut_clipboard" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_can_copy_clipboard" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_can_paste_clipboard" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_cut_clipboard" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_copy_clipboard" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_paste_clipboard" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_delete_selection" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_has_selection" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_select_all" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_get_editable" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_set_editable" :void
  (web-view webkit-web-view)
  (mode :boolean))

(defcfun "webkit_web_view_get_copy_target_list" gtk-target-list
  (web-view webkit-web-view))

(defcfun "webkit_web_view_get_paste_target_list" gtk-target-list
  (web-view webkit-web-view))

(defcfun "webkit_web_view_set_settings" :void
  (web-view webkit-web-view)
  (settings webkit-web-settings))

(defcfun "webkit_web_view_get_settings" webkit-web-settings
  (web-view webkit-web-view))

(defcfun "webkit_web_view_get_inspector" webkit-web-inspector
  (web-view webkit-web-view))

(defcfun "webkit_web_view_get_window_features" webkit-web-window-features
  (web-view webkit-web-view))

(defcfun "webkit_web_view_can_show_mime_type" :boolean
  (web-view webkit-web-view)
  (mime-type glib:g-string))

(defcfun "webkit_web_view_get_transparent" :void
  (web-view webkit-web-view)
  (mode :boolean))

(defcfun "webkit_web_view_get_zoom_level" :float
  (web-view webkit-web-view))

(defcfun "webkit_web_view_set_zoom_level" :void
  (web-view webkit-web-view)
  (level :float))

(defcfun "webkit_web_view_zoom_in" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_zoom_out" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_get_full_content_zoom" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_set_full_content_zoom" :void
  (web-view webkit-web-view)
  (full-content-zoom :boolean))

(defcfun "webkit_get_default_session" soup-session)

(defcfun "webkit_web_view_get_encoding" glib:g-string
  (web-view webkit-web-view))

(defcfun "webkit_web_view_set_custom_encoding" :void
  (web-view webkit-web-view)
  (encoding glib:g-string))

(defcfun "webkit_web_view_move_cursor" :void
  (web-view webkit-web-view)
  (step gtk-movement-step)
  (count :int))

(defcfun "webkit_web_view_get_load_status" webkit-load-status
  (web-view webkit-web-view))

(defcfun "webkit_web_view_get_progress" :double
  (web-view webkit-web-view))

(defcfun "webkit_web_view_undo" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_can_undo" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_redo" :void
  (web-view webkit-web-view))

(defcfun "webkit_web_view_can_redo" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_set_view_source_mode" :void
  (web-view webkit-web-view)
  (view-source-mode :boolean))

(defcfun "webkit_web_view_get_view_source_mode" :boolean
  (web-view webkit-web-view))

(defcfun "webkit_web_view_get_hit_test_result" webkit-hit-test-result
  (web-view webkit-web-view)
  (event gdk-event-button))

(defcfun "webkit_web_view_get_icon_uri" glib:g-string
  (web-view webkit-web-view))

(defcfun "webkit_set_cache_model" :void
  (cache-model webkit-cache-model))

(defcfun "webkit_get_cache_model" webkit-cache-model)
