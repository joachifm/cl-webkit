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
;;; webkitwebview.h

(include "/usr/include/webkit-1.0/webkit/webkitwebview.h")

(cenum webkit-navigation-response
       ((:webkit-navigation-response-accept "WEBKIT_NAVIGATION_RESPONSE_ACCEPT")
        :documentation "Accept response")
       ((:webkit-navigation-response-ignore "WEBKIT_NAVIGATION_RESPONSE_IGNORE")
        :documentation "Ignore response")
       ((:webkit-navigation-response-download "WEBKIT_NAVIGATION_RESPONSE_DOWNLOAD")
        :documentation "Download response"))

(cenum webkit-cache-model
       ((:webkit-cache-model-document-viewer "WEBKIT_CACHE_MODEL_DOCUMENT_VIEWER")
        :documentation "?")
       ((:webkit-cache-model-web-browser "WEBKIT_CACHE_MODEL_WEB_BROWSER")
        :documentation "?"))

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
