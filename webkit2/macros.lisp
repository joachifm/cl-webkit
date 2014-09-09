;;; macros.lisp --- here be dragons ...

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

;;; A more convenient wrapper for DEFINE-G-OBJECT-CLASS
;;;
;;; WebKit types have very regular naming, which means a lot of
;;; redundancy when defining classes (e.g., for a property zoom-level,
;;; we'd do (zoom-level prefix-zoom "zoom-level" ...).
;;; Exploit this pattern for great good.

(in-package :webkit2)

(defun parse-namespec (spec)
  "Parse namespec for PARSE-G-OBJECT-SLOT-SPEC.

Returns (values lisp-name g-property-name-string)."
  (etypecase spec
    (string (values (string->symbol spec)
                    spec))
    (list (values (car spec) (cadr spec)))))

(defun make-accessor-name (prefix name)
  "Make accessor name for PARSE-G-OBJECT-SLOT-SPEC."
  (check-type prefix symbol)
  (check-type name symbol)
  (string->symbol (concatenate 'string
                               (symbol-name prefix) "-" (symbol-name name))))

(defun parse-g-object-slot-spec (class-name spec)
  "Generate property slot specs for DEFINE-G-OBJECT-CLASS.

CLASS-NAME is the class prefix appended to the accessor
SPEC is (namespec type &optional readp writep), where
namespec is (or gname-string (list lisp-name-sym gname-string)),
type is the g-property type, and readp and writep indicate whether
to generate a reader/writer.

Unless explicitly specified, lisp-names are derived from gname-string."
  (check-type class-name symbol)
  (check-type spec list)
  ;; The orginal property spec syntax is (name accessor-name gname type readable writable)
  ;; cf. `parse-gobject-property' in cl-cffi-gtk/gobject/gobject.generating.lisp
  (destructuring-bind (namespec type &optional (readp t) (writep nil)) spec
    (multiple-value-bind (name gname) (parse-namespec namespec)
      (list name (make-accessor-name class-name name) gname type readp writep))))

;;; TODO: derive g-type-name from name (or the other round)
;;; TODO: derive type-initializer from name (or g-type-name)
;;; TODO: maybe support namespec (name gname &optional accessor-name)
;;; TODO: what about initarg?
(defmacro define-g-object-class* (g-type-name name
                                  (&key (superclass 'g-object)
                                        (export t)
                                        (interfaces nil)
                                        type-initializer)
                                  (&rest properties))
  "A variant of DEFINE-G-OBJECT-CLASS where property accessors and g-type names are
derived from the slot name.

See PARSE-G-OBJECT-SLOT-SPEC for the property syntax."
  `(define-g-object-class ,g-type-name ,name
     (:superclass ,superclass
      :export ,export
      :interfaces ,interfaces
      :type-initializer ,type-initializer)
     ,(mapcar #'(lambda (spec) (parse-g-object-slot-spec name spec)) properties)))
