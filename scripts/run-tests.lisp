;;; run-tests.lisp -- test suite driver

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :cl-user)

(require :asdf)

(setf asdf:*compile-file-warnings-behaviour* :error)

(handler-case (asdf:operate 'asdf:load-op :cl-webkit2-tests)
  (error (c) (uiop:die 1 "Failed: ~A~%" c)))

(let* ((result (lisp-unit:run-tests :all :cl-webkit2-tests))
       (failures (lisp-unit:failed-tests result)))
  (when (> (length failures) webkit2-tests:*expected-failure-count*)
    (uiop:die 1 "The number of failure exceeds the expected count.~%")))

(uiop:quit 0)
