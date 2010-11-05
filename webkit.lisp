(declaim (optimize (speed 0) (debug 3) (safety 3)))

(in-package :webkit)

;;; ^L

(define-foreign-library libwebkit
  (:unix (:or "libwebkit-1.0.so")))

(use-foreign-library libwebkit)

;;; ^L
;;; Dummies
;;;
;;; TODO: is this necessary?
;;; TODO: does cl-gtk2-glib provide foreign types that should be used?

(defctype gint :int)
(defctype guint :int)
(defctype g-type :pointer)
(defctype gchar :string)
(defctype gboolean :boolean)
(defctype gtk-target-list :pointer)
(defctype gfloat :float)
(defctype gdouble :double)
(defctype gtk-movement-step :int)
(defctype gdk-event-button :pointer)

;;; ^L
;;;
;;; TODO: What does WEBKIT_API work
;;; TODO: How does the G-OBJECT thing works

(defctype soup-session :pointer)

(defctype webkit-hit-test-result :pointer)

(defctype webkit-web-frame :pointer)

(defctype webkit-load-status :pointer)

(defctype webkit-web-window-features :pointer)

(defctype webkit-web-inspector :pointer)

(defctype webkit-web-settings :pointer)

(defctype webkit-web-back-forward-list :pointer)

(defctype webkit-web-history-item :pointer)

(defctype webkit-network-request :pointer)

;;; ^L
;;; webkitwebview.h

(defctype webkit-web-view :pointer)

(defcfun "webkit_web_view_get_type" g-type)

(defcfun "webkit_web_view_new" webkit-web-view)

(defcfun "webkit_web_view_get_title" gchar
  (view webkit-web-view))

(defcfun "webkit_web_view_get_uri" gchar
  (view webkit-web-view))

(defcfun "webkit_web_view_set_maintains_back_forward_list" :void
  (view webkit-web-view)
  (mode gboolean))

(defcfun "webkit_web_view_get_back_forward_list" webkit-web-back-forward-list
  (view webkit-web-view))

(defcfun "webkit_web_view_go_to_back_forward_item" gboolean
  (view webkit-web-view)
  (item webkit-web-history-item))

(defcfun "webkit_web_view_can_go_back" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_can_go_back_or_forward" gboolean
  (view webkit-web-view)
  (steps gint))

(defcfun "webkit_web_view_can_go_forward" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_go_forward" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_stop_loading" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_open" :void
  (view webkit-web-view)
  (uri gchar))

(defcfun "webkit_web_view_reload" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_reload_bypass_cache" :void
  (view webkit-web-view))

(defcfun ("webkit_web_view_load_uri" %webkit-web-view-load-uri) :void
  (view webkit-web-view)
  (uri :string))

(defun webkit-web-view-load-uri (view uri)
  (with-foreign-string (c-uri uri)
    (%webkit-web-view-load-uri view c-uri)))

(defcfun "webkit_web_view_load_string" :void
  (view webkit-web-view)
  (content gchar)
  (mime-type gchar)
  (encoding gchar)
  (base-uri gchar))

(defcfun "webkit_web_view_load_html_string" :void
  (view webkit-web-view)
  (content gchar)
  (base-uri gchar))

(defcfun "webkit_web_view_load_request" :void
  (view webkit-web-view)
  (request webkit-network-request))

(defcfun "webkit_web_view_search_text" gboolean
  (view webkit-web-view)
  (text gchar)
  (case-sensitive gboolean)
  (forward gboolean)
  (wrap gboolean))

(defcfun "webkit_web_view_mark_text_matches" guint
  (view webkit-web-view)
  (string gchar)
  (case-sensitive gboolean)
  (limit guint))

(defcfun "webkit_web_view_set_highlight_text_matches" :void
  (view webkit-web-view)
  (highlight gboolean))

(defcfun "webkit_web_view_unmark_text_matches" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_get_main_frame" webkit-web-frame
  (view webkit-web-view))

(defcfun "webkit_web_view_get_focused_frame" webkit-web-frame
  (view webkit-web-view))

(defcfun "webkit_web_view_execute_script" :void
  (view webkit-web-view)
  (script gchar))

(defcfun "webkit_web_view_can_cut_clipboard" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_can_copy_clipboard" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_can_paste_clipboard" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_cut_clipboard" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_copy_clipboard" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_paste_clipboard" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_delete_selection" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_has_selection" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_select_all" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_get_editable" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_set_editable" :void
  (view webkit-web-view)
  (mode gboolean))

(defcfun "webkit_web_view_get_copy_target_list" gtk-target-list
  (view webkit-web-view))

(defcfun "webkit_web_view_get_paste_target_list" gtk-target-list
  (view webkit-web-view))

(defcfun "webkit_web_view_set_settings" :void
  (view webkit-web-view)
  (settings webkit-web-settings))

(defcfun "webkit_web_view_get_settings" webkit-web-settings
  (view webkit-web-view))

(defcfun "webkit_web_view_get_inspector" webkit-web-inspector
  (view webkit-web-view))

(defcfun "webkit_web_view_get_window_features" webkit-web-window-features
  (view webkit-web-view))

(defcfun "webkit_web_view_can_show_mime_type" gboolean
  (view webkit-web-view)
  (mime-type gchar))

(defcfun "webkit_web_view_get_transparent" :void
  (view webkit-web-view)
  (mode gboolean))

(defcfun "webkit_web_view_get_zoom_level" gfloat
  (view webkit-web-view))

(defcfun "webkit_web_view_set_zoom_level" :void
  (view webkit-web-view)
  (level gfloat))

(defcfun "webkit_web_view_zoom_in" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_zoom_out" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_get_full_content_zoom" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_set_full_content_zoom" :void
  (view webkit-web-view)
  (full-content-zoom gboolean))

(defcfun "webkit_get_default_session" soup-session)

(defcfun "webkit_web_view_get_encoding" gchar
  (view webkit-web-view))

(defcfun "webkit_web_view_set_custom_encoding" :void
  (view webkit-web-view)
  (encoding gchar))

(defcfun "webkit_web_view_move_cursor" :void
  (view webkit-web-view)
  (step gtk-movement-step)
  (count gint))

(defcfun "webkit_web_view_get_load_status" webkit-load-status
  (view webkit-web-view))

(defcfun "webkit_web_view_get_progress" gdouble
  (view webkit-web-view))

(defcfun "webkit_web_view_undo" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_can_undo" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_redo" :void
  (view webkit-web-view))

(defcfun "webkit_web_view_can_redo" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_set_view_source_mode" :void
  (view webkit-web-view)
  (view-source-mode gboolean))

(defcfun "webkit_web_view_get_view_source_mode" gboolean
  (view webkit-web-view))

(defcfun "webkit_web_view_get_hit_test_result" webkit-hit-test-result
  (view webkit-web-view)
  (event gdk-event-button))

(defcfun "webkit_web_view_get_icon_uri" gchar
  (view webkit-web-view))

(defcfun "webkit_set_cache_model" :void
  (cache-model webkit-cache-model))

(defcfun "webkit_get_cache_model" webkit-cache-model)
