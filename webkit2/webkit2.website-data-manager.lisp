;;; webkit2.website-data-manager.lisp --- bindings for WebKitWebsiteDataManager

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package #:webkit2)

(define-webkit-class "WebKitWebsiteDataManager" ()
    (("base-cache-directory" "gchararray")
     ("base-data-directory" "gchararray")
     ("disk-cache-directory" "gchararray")
     ("hsts-cache-directory" "gchararray")
     ("indexeddb-directory" "gchararray")
     ("is-ephemeral" "gboolean")
     #+webkit2-tracking
     ("itp-directory" "gchararray")
     ("local-storage-directory" "gchararray")
     ("offline-application-cache-directory" "gchararray")))

(defctype webkit-website-data :pointer) ; XXX: GBoxed struct WebKitWebsiteData

#+webkit2-tracking
(defctype webkit-itp-first-party :pointer) ; XXX: GBoxed struct WebKitITPFirstParty

#+webkit2-tracking
(defctype webkit-itp-third-party :pointer) ; XXX: GBoxed struct WebKitITPThirdParty

(define-g-enum "WebKitWebsiteDataTypes" webkit-website-data-types ()
  :webkit-website-data-memory-cache
  :webkit-website-data-disk-cache
  :webkit-website-data-offline-application-cache
  :webkit-website-data-session-storage
  :webkit-website-data-local-storage
  :webkit-website-dataindexeddb-databases
  :webkit-website-data-plugin-data
  :webkit-website-data-cookies
  :webkit-website-data-device-id-hash-salt ;; From 2.24
  :webkit-website-data-hsts-cache          ;; From 2.26.
  :webkit-website-data-all)

(defcfun "webkit_website_data_ref" webkit-website-data
  (website-data webkit-website-data))
(export 'webkit-website-data-ref)

(defcfun "webkit_website_data_unref" webkit-website-data
  (website-data webkit-website-data))
(export 'webkit-website-data-unref)

(defcfun "webkit_website_data_get_name" :pointer ; XXX: const char *
  (website-data webkit-website-data))
(export 'webkit-website-data-get-name)

(defcfun "webkit_website_data_get_types" webkit-website-data-types
  (website-data webkit-website-data))
(export 'webkit-website-data-get-types)

(defcfun "webkit_website_data_get_size" :uint  ; guint64
  (website-data webkit-website-data)
  (types webkit-website-data-types))


(defcfun "webkit_website_data_manager_new_ephemeral" (g-object webkit-website-data-manager))
(export 'webkit-website-data-manager-new-ephemeral)

(defcfun "webkit_website_data_manager_get_cookie_manager" (g-object webkit-cookie-manager)
  (manager (g-object webkit-website-data-manager)))
(export 'webkit-website-data-manager-get-cookie-manager)

#+webkit2-tracking
(defcfun "webkit_website_data_manager_set_itp_enabled" :void
  (manager (g-object webkit-website-data-manager))
  (enabled :boolean))
#+webkit2-tracking
(export 'webkit-website-data-manager-set-itp-enabled)

#+webkit2-tracking
(defcfun "webkit_website_data_manager_get_itp_enabled" :boolean
  (manager (g-object webkit-website-data-manager)))
#+webkit2-tracking
(export 'webkit-website-data-manager-get-itp-enabled)

(defcfun "webkit_website_data_manager_fetch" :void
  (manager (g-object webkit-website-data-manager))
  (types webkit-website-data-types)
  (cancellable :pointer) ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-website-data-manager-fetch)

(defcfun "webkit_website_data_manager_fetch_finish" (glib:g-list webkit-website-data)
  (manager (g-object webkit-website-data-manager))
  (result g-async-result)
  (error (:pointer (:struct glib:g-error))))
(export 'webkit-website-data-manager-fetch-finish)

(defcfun "webkit_website_data_manager_remove" :void
  (manager (g-object webkit-website-data-manager))
  (types webkit-website-data-types)
  (website-data (glib:g-list webkit-website-data))
  (cancellable :pointer) ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-website-data-manager-remove)

(defcfun "webkit_website_data_manager_remove_finish" :boolean
  (manager (g-object webkit-website-data-manager))
  (result g-async-result)
  (error (:pointer (:struct glib:g-error))))
(export 'webkit-website-data-manager-remove-finish)

(defcfun "webkit_website_data_manager_clear" :void
  (manager (g-object webkit-website-data-manager))
  (types webkit-website-data-types)
  (timespan :uint) ;GTimeSpan
  (cancellable :pointer) ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
(export 'webkit-website-data-manager-clear)

(defcfun "webkit_website_data_manager_clear_finish" :boolean
  (manager (g-object webkit-website-data-manager))
  (result g-async-result)
  (error (:pointer (:struct glib:g-error))))
(export 'webkit-website-data-manager-clear-finish)

#+webkit2-tracking
(defcfun "webkit_itp_first_party_ref" webkit-itp-first-party
  (itp-first-party webkit-itp-first-party))
#+webkit2-tracking
(export 'webkit-itp-first-party-ref)

#+webkit2-tracking
(defcfun "webkit_itp_first_party_unref" :void
  (itp-first-party webkit-itp-first-party))
#+webkit2-tracking
(export 'webkit-itp-first-party-unref)

#+webkit2-tracking
(defcfun "webkit_itp_first_party_get_domain" :string
  (itp-first-party webkit-itp-first-party))
#+webkit2-tracking
(export 'webkit-itp-first-party-get-domain)

#+webkit2-tracking
(defcfun "webkit_itp_first_party_get_website_data_access_allowed" :boolean
  (itp-first-party webkit-itp-first-party))
#+webkit2-tracking
(export 'webkit-itp-first-party-get-website-data-access-allowed)

#+webkit2-tracking
(defcfun "webkit_itp_first_party_get_last_update_time" :pointer ; GDateTime *
  (itp-first-party webkit-itp-first-party))
#+webkit2-tracking
(export 'webkit-itp-first-party-get-last-update-time)

#+webkit2-tracking
(defcfun "webkit_itp_third_party_ref" webkit-itp-third-party
  (itp-third-party webkit-itp-third-party))
#+webkit2-tracking
(export 'webkit-itp-third-party-ref)

#+webkit2-tracking
(defcfun "webkit_itp_third_party_unref" :void
  (itp-third-party webkit-itp-third-party))
#+webkit2-tracking
(export 'webkit-itp-third-party-unref)

#+webkit2-tracking
(defcfun "webkit_itp_third_party_get_domain" :string
  (itp-third-party webkit-itp-third-party))
#+webkit2-tracking
(export 'webkit-itp-third-party-get-domain)

#+webkit2-tracking
(defcfun "webkit_itp_third_party_get_first_parties" (glib:g-list webkit-itp-first-party)
  (itp-third-party webkit-itp-third-party))
#+webkit2-tracking
(export 'webkit-itp-third-party-get-first-parties)

#+webkit2-tracking
(defcfun "webkit_website_data_manager_get_itp_summary" :void
  (manager (g-object webkit-website-data-manager))
  (cancellable :pointer) ; GCancellable
  (callback g-async-ready-callback)
  (user-data :pointer))
#+webkit2-tracking
(export 'webkit-website-data-manager-get-itp-summary)

#+webkit2-tracking
(defcfun "webkit_website_data_manager_get_itp_summary_finish" (glib:g-list webkit-itp-first-party)
  (manager (g-object webkit-website-data-manager))
  (result g-async-result)
  (error (:pointer (:struct glib:g-error))))
#+webkit2-tracking
(export 'webkit-website-data-manager-get-itp-summary-finish)
