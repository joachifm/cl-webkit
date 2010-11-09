;; buildtest.lisp
;;
;; Commentary:
;;
;; Attempt to load a system, report outcome to the OS.
;; Used in git_hooks/pre-commit
;;
;; Usage:
;;
;;     $ lisp --eval '(progn (load "buildtest.lisp") (buildtest SYSTEM))'
;;
;; This assumes that SYSTEM's definition is in the CWD and that lisp has ASDF.
;;
;; Code:

(defun exit (code)
  #+ccl (quit code)
  #+clisp (ext:exit code)
  #+ecl (ext:exit code)
  #+sbcl (sb-unix:unix-exit code))

(eval-when (:load-toplevel :compile-toplevel)
  (unless (find-package :asdf)
    (format t "ASDF is required to run this script.")
    (exit 1)))

(defun buildtest (sys)
  (let* ((null-stream (make-broadcast-stream))
         (*standard-output* null-stream)
         (*trace-output* null-stream))
    (handler-case (asdf:operate 'asdf:load-op sys)
      (error (c) (progn (format *error-output* "Failed: ~A~%" c)
                        (exit 1)))))

  (exit 0))
