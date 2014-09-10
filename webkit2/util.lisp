;;; util.lisp --- general purpose utilities for internal use

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2)

(defun foo->symbol (&rest lst)
  "Convert a list of things (coercible to string) into a symbol."
  (let ((s (apply #'concatenate 'string (mapcar #'string lst))))
    (intern (string-upcase s)
            (load-time-value (find-package ':cl-webkit2)))))

(declaim (inline char-upcase-ascii-letter-p))
(defun char-upcase-ascii-letter-p (c)
  (check-type c character)
  (let ((code (char-code c)))
    (and (>= code 65)
         (<= code 90))))

;;; TODO: replace SINGLE uppercase letters, not if they are consequtive
;;;       e.g., break WebView into web-view but WebURI should be web-uri
(defun medial->delim (input &key (delim #\-) (start 0) (end nil) (norm-case #'char-downcase))
  "Convert medial capitalization to char-delimited."
  (check-type input string)
  (with-output-to-string (buf)
    (with-input-from-string (s input :start start :end end)
      ;; first char is special
      (let ((c (read-char s)))
        (write-char (if (char-upcase-ascii-letter-p c)
                        (funcall norm-case c)
                        c)
                    buf))
      ;; loop over remainder
      (do ((c (read-char s nil nil) (read-char s nil nil)))
          ((null c) buf)
        (cond ((char-upcase-ascii-letter-p c)
               (write-char delim buf)
               (write-char (funcall norm-case c) buf))
              (t
               (write-char (funcall norm-case c) buf)))))))
