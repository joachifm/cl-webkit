(in-package :webkit)

(define-foreign-library libwebkit
  (:unix (:or "libwebkit-1.0.so")))

(use-foreign-library libwebkit)

;;; ^L
;;; webkitwebview.h

(defctype web-view :pointer)

(defcfun "webkit_web_view_new" web-view)

(defcfun ("webkit_web_view_load_uri" %webkit-web-view-load-uri) :void
  (view web-view)
  (uri :string))

(defun webkit-web-view-load-uri (view uri)
  (with-foreign-string (c-uri uri)
    (%webkit-web-view-load-uri view c-uri)))

(defcfun "webkit_web_view_get_title" :string
  (view web-view))

(defcfun "webkit_web_view_get_editable" :boolean
  (view web-view))

(defcfun "webkit_web_view_set_editable" :void
  (view web-view)
  (mode :boolean))
