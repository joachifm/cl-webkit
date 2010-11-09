;;;; buildtest.lisp: catch load/compile errors
;;;; Used by git_hooks/pre-commit

;; Where to get dependencies. Change to suit your needs.
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#-asdf
(require :asdf)

;; Load the system, signal error on failure.
;; Note that warnings and so on are ignored.
;; All other output is redirected to /dev/null.

(defun exit (code)
  #+ccl (quit code)
  #+clisp (ext:exit code)
  #+ecl (ext:exit code)
  #+sbcl (sb-unix:unix-exit code))

(let* ((null-stream (make-broadcast-stream))
       (*standard-output* null-stream)
       (*trace-output* null-stream))
  (handler-case (asdf:operate 'asdf:load-op :webkit)
    (error (c) (progn (format *error-output* "Failed: ~A~%" c)
                      (exit 1)))))

;; If we got here, everything went well
(exit 0)
