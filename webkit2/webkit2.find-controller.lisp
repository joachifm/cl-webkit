;;; webkit2.find-controller.lisp --- bindings for WebKitFindController

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitFindController" ()
  (("max-match-count" "guint")
   ("text" "gchararray")
   ("web-view" "WebKitWebView" t t)))

(define-g-enum "WebKitFindOptions" webkit-find-options ()
  :webkit-find-options-none
  (:webkit-find-options-case-insensitive 1)
  (:webkit-find-options-at-word-starts 2)
  (:webkit-find-options-treat-medial-capital-as-word-start 4)
  (:webkit-find-options-backwards 8)
  (:webkit-find-options-wrap-around 16))

(defcfun ("webkit_find_controller_search" %webkit-find-controller-search) :void
  (find-controller (g-object webkit-find-controller))
  (search-text :string)
  (find-options :uint)
  (max-match-count :uint))

(defun webkit-find-controller-search (controller search-text
                                      &key (max-match-count
                                            ;; I.e., UINT_MAX
                                            (1- (expt 2 (* 8 (cffi:foreign-type-size :uint)))))
                                        (case-insensitive t)
                                        at-word-starts
                                        treat-medial-capital-as-word-start
                                        backwards
                                        (wrap-around t))
  "A smarter version of webkit_find_controller_search with sensible defaults.
CONTROLLER is a `webkit:webkit-find-controller'.
SEARCH-TEXT is a string to search for.
MAX-MATCH-COUNT is the maximum match-count. Set to the max-match-count of CONTROLLER by default.
Other keys regulate search. CASE-INSENSITIVE and WRAP-AROUND are set to true by default."
  (%webkit-find-controller-search
   controller search-text
   (logior
    (if case-insensitive
        (cffi:foreign-enum-value 'webkit-find-options :webkit-find-options-case-insensitive)
        0)
    (if at-word-starts
        (cffi:foreign-enum-value 'webkit-find-options :webkit-find-options-at-word-starts)
        0)
    (if treat-medial-capital-as-word-start
        (cffi:foreign-enum-value 'webkit-find-options
                                 :webkit-find-options-treat-medial-capital-as-word-start)
        0)
    (if backwards
        (cffi:foreign-enum-value 'webkit-find-options :webkit-find-options-backwards)
        0)
    (if wrap-around
        (cffi:foreign-enum-value 'webkit-find-options :webkit-find-options-wrap-around)
        0))
   max-match-count))
(export 'webkit-find-controller-search)

(defcfun "webkit_find_controller_search_next" :void
  (find-controller (g-object webkit-find-controller)))
(export 'webkit-find-controller-search-next)

(defcfun "webkit_find_controller_search_previous" :void
  (find-controller (g-object webkit-find-controller)))
(export 'webkit-find-controller-search-previous)

(defcfun "webkit_find_controller_search_finish" :void
  (find-controller (g-object webkit-find-controller)))
(export 'webkit-find-controller-search-finish)
