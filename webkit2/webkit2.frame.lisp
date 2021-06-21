(in-package #:webkit2)

(define-webkit-class "WebKitFrame" () ())

(defcfun "webkit_frame_is_main_frame" :boolean
  (frame (g-object webkit-frame)))
(export 'webkit-frame-is-main-frame)

(defcfun "webkit_frame_get_uri" :string
  (frame (g-object webkit-frame)))
(export 'webkit-frame-get-uri)

(defctype js-global-context-ref :pointer)
(export 'js-global-context-ref)

(defcfun "webkit_frame_get_javascript_global_context" js-global-context-ref
  (frame (g-object webkit-frame)))
(export 'webkit-frame-get-javascript-global-context)

(defcfun "webkit_frame_get_javascript_context_for_script_world" js-global-context-ref
  (frame (g-object webkit-frame))
  (world (g-object webkit-script-world)))
(export 'webkit-frame-get-javascript-context-for-script-world)

(defcfun "webkit_frame_get_js_value_for_dom_object" (g-object jsc-value)
  (frame (g-object webkit-frame))
  (dom-object (g-object webkit-dom-object)))
(export 'webkit-frame-get-js-value-for-dom-object)

(defcfun "webkit_frame_get_js_value_for_dom_object_in_script_world" (g-object jsc-value)
  (frame (g-object webkit-frame))
  (dom-object (g-object webkit-dom-object))
  (world (g-object webkit-script-world)))
(export 'webkit-frame-get-js-value-for-dom-object-in-script-world)
