(in-package #:jscore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @typedef JSContextRef A JavaScript execution context. Holds the global object and other execution state.
;; typedef const struct OpaqueJSContext* JSContextRef;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @typedef JSValueRef A JavaScript value. The base type for all JavaScript values, and polymorphic functions on them. */
;; typedef const struct OpaqueJSValue* JSValueRef;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @typedef JSStringRef A UTF16 character buffer. The fundamental string representation in JavaScript.
;; typedef struct OpaqueJSString* JSStringRef;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @abstract        Converts a JavaScript value to string and copies the result into a JavaScript string.
;; @param ctx       The execution context to use.
;; @param value     The JSValue to convert.
;; @param exception A pointer to a JSValueRef in which to store an exception, if any. Pass NULL if you do not care to store an exception.
;; @result          A JSString with the result of conversion, or NULL if an exception is thrown. Ownership follows the Create Rule.
;; JS_EXPORT JSStringRef JSValueToStringCopy(JSContextRef ctx, JSValueRef value, JSValueRef* exception);

(cffi:defctype js-context-ref :pointer)
(cffi:defctype js-value-ref :pointer)
(cffi:defctype js-string-ref :pointer)

(cffi:defcfun ("JSValueToStringCopy" js-value-to-string-copy) :string
  (ctx js-context-ref)
  (value js-value-ref)
  (exception :pointer))

(export 'js-value-to-string-copy)

(cffi:defcfun ("JSStringGetMaximumUTF8CStringSize" js-string-get-maximum-utf-8-c-string-size) :unsigned-int
  (string js-string-ref))

(export 'js-string-get-maximum-utf-8-c-string-size)
