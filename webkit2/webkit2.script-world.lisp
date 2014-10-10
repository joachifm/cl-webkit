;;; webkit2.script-world.lisp --- bindings for WebKitScriptWorld

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(define-webkit-class "WebKitScriptWorld" () ())

(defcfun "webkit_script_world_get_default" (g-object webkit-script-world))
(export 'webkit-script-world-get-default)

;;; XXX: expose this until we're sure that make-instance does the same
;;; thing, e.g., create an /isolated/ script world.
(defcfun "webkit_script_world_new" (g-object webkit-script-world))
(export 'webkit-script-world-new)
