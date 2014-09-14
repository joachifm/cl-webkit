;;; webkit2.plugin.lisp --- bindings for WebKitPlugin

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitPlugin" () ())

(defcfun "webkit_plugin_get_name" :string
  (plugin (g-object webkit-plugin)))
(export 'webkit-plugin-get-name)

(defcfun "webkit_plugin_get_description" :string
  (plugin (g-object webkit-plugin)))
(export 'webkit-plugin-get-description)

(defcfun "webkit_plugin_get_path" :string
  (plugin (g-object webkit-plugin)))
(export 'webkit-plugin-get-path)
