;;; webkit2.init.lisp --- initialize foreign library

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(glib::at-init ()
  (define-foreign-library libwebkit2
    (:darwin (:or
              "libwebkit2gtk-4.1.dylib"
              "libwebkit2gtk-4.0.37.39.3.dylib"
              "libwebkit2gtk-4.0.37.dylib"
              "libwebkit2gtk-4.0.dylib"))
    (:unix (:or
            "libwebkit2gtk-4.1.so"
            "libwebkit2gtk-4.0.so"
            ;; Fedora only has this one?
            "libwebkit2gtk-4.0.so.37")))
  (use-foreign-library libwebkit2))

(defcfun "webkit_get_major_version" :int)
(defcfun "webkit_get_minor_version" :int)

(eval-when (:load-toplevel :execute)
  (with-standard-io-syntax
    (let* ((version (format nil "~a.~a"
                            (webkit-get-major-version)
                            (webkit-get-minor-version)))
           (versym (intern (format nil "WEBKIT2-~a" version)
                           :keyword)))
      (when (uiop:version<= "2.26" version)
        ;; Sandboxing only works on Linux as of 2.28.  See
        ;; https://webkitgtk.org/reference/webkit2gtk/unstable/WebKitWebContext.html.
        ;; TODO: Only set feature on Linux?
        (dolist (sym '("WEBKIT2-SANDBOXING" "WEBKIT2-MEDIA" "WEBKIT2-EMOJI"))
          (pushnew (intern sym :keyword) *features*)))
      (when (uiop:version<= "2.30" version)
        (dolist (sym '("WEBKIT2-MUTE" "WEBKIT2-TRACKING" "WEBKIT2-PASTE-PLAINTEXT"))
          (pushnew (intern sym :keyword) *features*)))
      (when (uiop:version<= "2.34" version)
        (dolist (sym '("WEBKIT2-CORS-ALLOWLIST"))
          (pushnew (intern sym :keyword) *features*)))
      (pushnew versym *features*)))

  (pushnew :WEBKIT2 *features*))
