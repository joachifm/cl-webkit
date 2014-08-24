;;; grovel.lisp - grovel script
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

;;; Commentary:
;;
;; Used by CFFI-grovel to automatically extract information about types and
;; structures.

;;; Code:

(in-package :cl-webkit.foreign)

;; XXX: this should be auto-generated
(cc-flags
 "-I/usr/include/webkitgtk-3.0"
 "-I/usr/include/glib-2.0"
 "-I/usr/lib/glib-2.0/include"
 "-I/usr/include/gtk-2.0"
 "-I/usr/include/libsoup-2.4"
 "-I/usr/lib/gtk-2.0/include"
 "-I/usr/include/atk-1.0"
 "-I/usr/include/cairo"
 "-I/usr/include/gdk-pixbuf-2.0"
 "-I/usr/include/pango-1.0"
 "-I/usr/include/pixman-1"
 "-I/usr/include/freetype2"
 "-I/usr/include/libpng14"
 "-I/usr/include/libxml2")

;;; webkiterror.h

(include "webkit/webkiterror.h")

(cenum webkit-network-error
       ((:webkit-network-error-failed "WEBKIT_NETWORK_ERROR_FAILED")
        :documentation "?")
       ((:webkit-network-error-transport "WEBKIT_NETWORK_ERROR_TRANSPORT")
        :documentation "?")
       ((:webkit-network-error-unknown-protocol "WEBKIT_NETWORK_ERROR_UNKNOWN_PROTOCOL")
        :documentation "?")
       ((:webkit-network-error-cancelled "WEBKIT_NETWORK_ERROR_CANCELLED")
        :documentation "?")
       ((:webkit-network-error-file-does-not-exist "WEBKIT_NETWORK_ERROR_FILE_DOES_NOT_EXIST")
        :documentation "?"))

(cenum webkit-policy-error
       ((:webkit-policy-error-failed "WEBKIT_POLICY_ERROR_FAILED")
        :documentation "?")
       ((:webkit-policy-error-cannot-show-mime-type "WEBKIT_POLICY_ERROR_CANNOT_SHOW_MIME_TYPE")
        :documentation "?")
       ((:webkit-policy-error-cannot-show-url "WEBKIT_POLICY_ERROR_CANNOT_SHOW_URL")
        :documentation "?")
       ((:webkit-policy-error-frame-load-interrupted-by-policy-change "WEBKIT_POLICY_ERROR_FRAME_LOAD_INTERRUPTED_BY_POLICY_CHANGE")
        :documentation "?")
       ((:webkit-policy-error-cannot-use-restricted-port "WEBKIT_POLICY_ERROR_CANNOT_USE_RESTRICTED_PORT")
        :documentation "?"))

(cenum webkit-plugin-error
       ((:webkit-plugin-error-failed "WEBKIT_PLUGIN_ERROR_FAILED")
        :documentation "?")
       ((:webkit-plugin-error-cannot-find-plugin "WEBKIT_PLUGIN_ERROR_CANNOT_FIND_PLUGIN")
        :documentation "?")
       ((:webkit-plugin-error-cannot-load-plugin "WEBKIT_PLUGIN_ERROR_CANNOT_LOAD_PLUGIN")
        :documentation "?")
       ((:webkit-plugin-error-java-unavailable "WEBKIT_PLUGIN_ERROR_JAVA_UNAVAILABLE")
        :documentation "?")
       ((:webkit-plugin-error-connection-cancelled "WEBKIT_PLUGIN_ERROR_CONNECTION_CANCELLED")
        :documentation "?")
       ((:webkit-plugin-error-will-handle-load "WEBKIT_PLUGIN_ERROR_WILL_HANDLE_LOAD")
        :documentation "?"))


;;; webkitwebnavigationaction.h

(include "webkit/webkitwebnavigationaction.h")

(cenum webkit-web-navigation-reason
       ((:webkit-web-navigation-reason-link-clicked "WEBKIT_WEB_NAVIGATION_REASON_LINK_CLICKED")
        :documentation "?")
       ((:webkit-web-navigation-reason-form-submitted "WEBKIT_WEB_NAVIGATION_REASON_FORM_SUBMITTED")
        :documentation "?")
       ((:webkit-web-navigation-reason-back-forward "WEBKIT_WEB_NAVIGATION_REASON_BACK_FORWARD")
        :documentation "?")
       ((:webkit-web-navigation-reason-reload "WEBKIT_WEB_NAVIGATION_REASON_RELOAD")
        :documentation "?")
       ((:webkit-web-navigation-reason-form-resubmitted "WEBKIT_WEB_NAVIGATION_REASON_FORM_RESUBMITTED")
        :documentation "?")
       ((:webkit-web-navigation-reason-other "WEBKIT_WEB_NAVIGATION_REASON_OTHER")
        :documentation "?"))

;;; ^L
;;; webkitdownload.h

(include "webkit/webkitdownload.h")

(cenum webkit-download-status
       ((:webkit-download-status-error "WEBKIT_DOWNLOAD_STATUS_ERROR")
        :documentation "?")
       ((:webkit-download-status-created "WEBKIT_DOWNLOAD_STATUS_CREATED")
        :documentation "?")
       ((:webkit-download-status-started "WEBKIT_DOWNLOAD_STATUS_STARTED")
        :documentation "?")
       ((:webkit-download-status-cancelled "WEBKIT_DOWNLOAD_STATUS_CANCELLED")
        :documentation "?")
       ((:webkit-download-status-finished "WEBKIT_DOWNLOAD_STATUS_FINISHED")
        :documentation "?"))

(cenum webkit-download-error
       ((:webkit-download-error-cancelled-by-user "WEBKIT_DOWNLOAD_ERROR_CANCELLED_BY_USER")
        :documentation "?")
       ((:webkit-download-error-destination "WEBKIT_DOWNLOAD_ERROR_DESTINATION")
        :documentation "?")
       ((:webkit-download-error-network "WEBKIT_DOWNLOAD_ERROR_NETWORK")
        :documentation "?"))

;;; webkitwebsettings.h

(include "webkit/webkitwebsettings.h")

(cenum webkit-editing-behavior
       ((:webkit-editing-behavior-mac "WEBKIT_EDITING_BEHAVIOR_MAC")
        :documentation "?")
       ((:webkit-editing-behavior-windows "WEBKIT_EDITING_BEHAVIOR_WINDOWS")
        :documentation "?")
       ((:webkit-editing-behavior-unix "WEBKIT_EDITING_BEHAVIOR_UNIX")
        :documentation "?"))

;;; webkithittestresult.h

(include "webkit/webkithittestresult.h")

(cenum webkit-hit-test-result-context
       ((:webkit-hit-test-result-context-document "WEBKIT_HIT_TEST_RESULT_CONTEXT_DOCUMENT")
        :documentation "Anywhere in the document.")
       ((:webkit-hit-test-result-context-link "WEBKIT_HIT_TEST_RESULT_CONTEXT_LINK")
        :documentation "A hyperlink element.")
       ((:webkit-hit-test-result-context-image "WEBKIT_HIT_TEST_RESULT_CONTEXT_IMAGE")
        :documentation "An image element.")
       ((:webkit-hit-test-result-context-media "WEBKIT_HIT_TEST_RESULT_CONTEXT_MEDIA")
        :documentation "A video or audio element.")
       ((:webkit-hit-test-result-context-selection "WEBKIT_HIT_TEST_RESULT_CONTEXT_SELECTION")
        :documentation "The area is selected by the user.")
       ((:webkit-hit-test-result-context-editable "WEBKIT_HIT_TEST_RESULT_CONTEXT_EDITABLE")
        :documentation "The area is editable by the user."))

;;; webkitwebframe.h

(include "webkit/webkitwebframe.h")

(cenum webkit-load-status
       ((:webkit-load-provisional "WEBKIT_LOAD_PROVISIONAL")
        :documentation "No data has been received yet, empty structurs have
been allocation to perform the load; the load may still fail for transport
issues such as not being able to resolve a name, or connect to a port.")
       ((:webkit-load-committed "WEBKIT_LOAD_COMMITTED")
        :documentation "The first data chunk has arrived, meaning that the
necessary transport requirements are established, and the load is being
performed.")
       ((:webkit-load-first-visually-non-empty-layout "WEBKIT_LOAD_FIRST_VISUALLY_NON_EMPTY_LAYOUT")
        :documentation "The first layout with actual visible content happened;
one or more layouts may have happened before tht caused nothing to be visible
on the screen, because the data available at the time was not significant enough.")
       ((:webkit-load-finished "WEBKIT_LOAD_FINISHED")
        :documentation "Everything required to display the page has been loaded.")
       ((:webkit-load-failed "WEBKIT_LOAD_FAILED")
        :documentation "Page failed to load. Connect to the load-error signal
if you want to know precisely what kind of error occurred."))

;;; webkitwebview.h

(include "webkit/webkitwebview.h")

(cenum webkit-navigation-response
       ((:webkit-navigation-response-accept "WEBKIT_NAVIGATION_RESPONSE_ACCEPT")
        :documentation "Accept response")
       ((:webkit-navigation-response-ignore "WEBKIT_NAVIGATION_RESPONSE_IGNORE")
        :documentation "Ignore response")
       ((:webkit-navigation-response-download "WEBKIT_NAVIGATION_RESPONSE_DOWNLOAD")
        :documentation "Download response"))

(cenum webkit-web-view-target-info
       ((:webkit-web-view-target-info-html "WEBKIT_WEB_VIEW_TARGET_INFO_HTML")
        :documentation "?")
       ((:webkit-web-view-target-info-text "WEBKIT_WEB_VIEW_TARGET_INFO_TEXT")
        :documentation "?")
       ((:webkit-web-view-target-info-image "WEBKIT_WEB_VIEW_TARGET_INFO_IMAGE")
        :documentation "?")
       ((:webkit-web-view-target-info-uri-list "WEBKIT_WEB_VIEW_TARGET_INFO_URI_LIST")
        :documentation "?")
       ((:webkit-web-view-target-info-netscape-url "WEBKIT_WEB_VIEW_TARGET_INFO_NETSCAPE_URL")
        :documentation "?"))

(cenum webkit-web-view-view-mode
       ((:webkit-web-view-view-mode-windowed "WEBKIT_WEB_VIEW_VIEW_MODE_WINDOWED")
        :documentation "?")
       ((:webkit-web-view-view-mode-floating "WEBKIT_WEB_VIEW_VIEW_MODE_FLOATING")
        :documentation "?")
       ((:webkit-web-view-view-mode-fullscreen "WEBKIT_WEB_VIEW_VIEW_MODE_FULLSCREEN")
        :documentation "?")
       ((:webkit-web-view-view-mode-maximized "WEBKIT_WEB_VIEW_VIEW_MODE_MAXIMIZED")
        :documentation "?")
       ((:webkit-web-view-view-mode-minimized "WEBKIT_WEB_VIEW_VIEW_MODE_MINIMIZED")
        :documentation "?"))

(cenum webkit-selection-affinity
       ((:webkit-selection-affinity-upstream "WEBKIT_SELECTION_AFFINITY_UPSTREAM")
        :documentation "?")
       ((:webkit-selection-affinity-downstream "WEBKIT_SELECTION_AFFINITY_DOWNSTREAM")
        :documentation "?"))

(cenum webkit-insert-action
       ((:webkit-insert-action-typed "WEBKIT_INSERT_ACTION_TYPED")
        :documentation "?")
       ((:webkit-insert-action-pasted "WEBKIT_INSERT_ACTION_PASTED")
        :documentation "?")
       ((:webkit-insert-action-dropped "WEBKIT_INSERT_ACTION_DROPPED")
        :documentation "?"))


;;; webkitglobals.h

(include "webkit/webkitglobals.h")

;; The cache model determines the memory and disk space to use for caching content.
(cenum webkit-cache-model
       ((:webkit-cache-model-default "WEBKIT_CACHE_MODEL_DEFAULT")
        :documentation "The default cache model (optimized for web browsing).")
       ((:webkit-cache-model-document-viewer "WEBKIT_CACHE_MODEL_DOCUMENT_VIEWER")
        :documentation "Use this for applications without a browsing inteface.")
       ((:webkit-cache-model-document-browser "WEBKIT_CACHE_MODEL_DOCUMENT_BROWSER")
        :documentation "Cache model optimized for viewing.")
       ((:webkit-cache-model-web-browser "WEBKIT_CACHE_MODEL_WEB_BROWSER")
        :documentation "Use this for web browser applications."))
