;;; run-tests.lisp -- test suite driver

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :cl-user)

(require :asdf)

(defparameter *expected-failure-count* 3)

(handler-case (asdf:operate 'asdf:load-op :cl-webkit2-tests)
  (error (c) (progn (format *error-output* "Failed: ~A~%" c)
                    (sb-ext:exit :code 1))))
(let* ((result (lisp-unit:run-tests :all :cl-webkit2-tests))
       (failures (lisp-unit:failed-tests result)))
  (when (> (length failures)
           *expected-failure-count*)
    (format *error-output* "The number of failures exceeds the expected count.~%")
    (sb-ext:exit :code 1)))
(sb-ext:exit :code 0)
