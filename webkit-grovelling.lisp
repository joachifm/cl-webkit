(in-package :webkit)

(cc-flags
 "-I/usr/include/webkit-1.0"
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

;;; ^L
;;; webkitwebframe.h

(include "/usr/include/webkit-1.0/webkit/webkitwebframe.h")

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

;;; ^L
;;; webkitwebview.h

(include "/usr/include/webkit-1.0/webkit/webkitwebview.h")

(cenum webkit-navigation-response
       ((:webkit-navigation-response-accept "WEBKIT_NAVIGATION_RESPONSE_ACCEPT")
        :documentation "Accept response")
       ((:webkit-navigation-response-ignore "WEBKIT_NAVIGATION_RESPONSE_IGNORE")
        :documentation "Ignore response")
       ((:webkit-navigation-response-download "WEBKIT_NAVIGATION_RESPONSE_DOWNLOAD")
        :documentation "Download response"))

;; The cache model determines the memory and disk space to use for caching content.
(cenum webkit-cache-model
       ((:webkit-cache-model-document-viewer "WEBKIT_CACHE_MODEL_DOCUMENT_VIEWER")
        :documentation "Use this for applications without a browsing inteface.")
       ((:webkit-cache-model-web-browser "WEBKIT_CACHE_MODEL_WEB_BROWSER")
        :documentation "Use this for web browser applications."))

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
