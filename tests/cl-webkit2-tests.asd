;;; cl-webkit2-tests.asd --- ASDF system definition for the test-suite

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :asdf-user)

(defsystem :cl-webkit2-tests
  :serial t
  :components ((:file "package")
               (:file "lisp-unit" :depends-on ("package")))
  :depends-on (:cl-webkit2 :lisp-unit))
