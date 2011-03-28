;;; cl-webkit.asd - system definitions
;; Copyright (C) 2010, Joachim Fasting
;;
;;   This file is part of cl-webkit
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistribution of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistribution in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cffi-grovel))

(defpackage :cl-webkit-asd
  (:use :cl :asdf))

(in-package :cl-webkit-asd)

(defsystem :cl-webkit
  :description "A simple FFI for WebKitGtk+"
  :author "Joachim Fasting <joachim.fasting@gmail.com>"
  :licence "BSD"
  :version "0.0"
  :serial t
  :components ((:file "packages")
               (cffi-grovel:grovel-file "grovel")
               (:file "foreign"))
  :depends-on (:cffi :cl-gtk2-glib))
