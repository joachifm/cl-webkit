;;; buildtest.lisp

;;; Commentary:
;;
;; Attempt to load a system, report outcome to the OS.
;; Used in git_hooks/pre-commit
;;
;; Usage:
;;
;;     $ lisp --eval '(progn (load "buildtest.lisp") (buildtest SYSTEM))'

;;; Code:

(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel)
  (unless (find-package :asdf)
    (format t "ASDF is required to run this script.")
    (sb-ext:exit :code 1)))

(defun buildtest (sys)
  (let* ((null-stream (make-broadcast-stream))
         (*standard-output* null-stream)
         (*trace-output* null-stream))
    (handler-case (asdf:operate 'asdf:load-op sys)
      (error (c) (progn (format *error-output* "Failed: ~A~%" c)
                        (sb-ext:exit :code 1)))))
  (sb-ext:exit :code 0))
