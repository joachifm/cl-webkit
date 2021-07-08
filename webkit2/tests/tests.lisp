;;; tests/tests.lisp -- Testing code.

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Code:

(in-package :webkit2/tests)

(def-suite webkit-tests :description "Testing WebKit.")

(def-suite js-tests :description "Testing JS value transformation." :in webkit-tests)

(defvar *webkit-environment* (make-hash-table :test 'equal))

(defmacro with-js-transform-result (js-string (var &optional (jsc-var (gensym))
                                                     (context-var (gensym)))
                                    &body body)
  `(let ((channel (make-instance
                   'calispel:channel
                   :buffer (make-instance 'jpl-queues:bounded-fifo-queue :capacity 1))))
     (gtk:within-gtk-thread
       (webkit2:webkit-web-view-evaluate-javascript
        (gethash "view" *webkit-environment*) ,js-string
        (lambda (result jsc-value)
          (calispel:! channel result)
          (calispel:! channel jsc-value)
          (calispel:! channel (webkit:jsc-value-get-context jsc-value)))))
     (let ((,var (calispel:? channel))
           (,jsc-var (calispel:? channel))
           (,context-var (calispel:? channel)))
       ,@body)))

(gtk:within-main-loop
  (let* ((win (make-instance 'gtk:gtk-window))
         (manager (make-instance 'webkit:webkit-website-data-manager
                                 :base-data-directory "testing-data-manager"))
         (context (make-instance 'webkit:webkit-web-context
                                 :website-data-manager manager))
         (view (make-instance 'webkit2:webkit-web-view
                              :web-context context)))
    (gobject:g-signal-connect win "destroy"
                              #'(lambda (widget)
                                  (declare (ignore widget))
                                  (gtk:leave-gtk-main)))
    (gtk:gtk-container-add win view)
    (webkit2:webkit-web-view-load-uri view "http://www.example.com")
    (setf (gethash "win" *webkit-environment*) win
          (gethash "manager" *webkit-environment*) manager
          (gethash "context" *webkit-environment*) context
          (gethash "view" *webkit-environment*) view)))

;;; Literal types

(def-test undefined (:suite js-tests)
  (with-js-transform-result "undefined" (%result%)
    (is (eq :undefined %result%))))

(def-test null (:suite js-tests)
  (with-js-transform-result "null" (%result%)
    (is (eq :null %result%))))

(def-test null-as-nil (:suite js-tests)
  (setf webkit::*js-null-value* nil)
  (with-js-transform-result "null" (%result%)
    (is (eq nil %result%)))
  (setf webkit::*js-null-value* :null))

(def-test false (:suite js-tests)
  (with-js-transform-result "false" (%result%)
    (is (eq nil %result%))))

(def-test false-as-keyword (:suite js-tests)
  (setf webkit::*js-false-value* :false)
  (with-js-transform-result "false" (%result%)
    (is (eq :false %result%)))
  (setf webkit::*js-false-value* nil))

(def-test true (:suite js-tests)
  (with-js-transform-result "true" (%result%)
    (is (eq t %result%))))

(def-test true-as-keyword (:suite js-tests)
  (setf webkit::*js-true-value* :true)
  (with-js-transform-result "true" (%result%)
    (is (eq :true %result%)))
  (setf webkit::*js-true-value* t))

;;; Numbers

(def-test integers (:suite js-tests)
  (with-js-transform-result "0" (%result%)
    (is (= 0 %result%)))
  (with-js-transform-result "8" (%result%)
    (is (= 8 %result%)))
  (with-js-transform-result "-6" (%result%)
    (is (= -6 %result%)))
  (with-js-transform-result "Math.pow(10, 100)" (%result%)
    (is (= 1.0000000000000002d100 %result%)))
  (with-js-transform-result "-Math.pow(10, 100)" (%result%)
    (is (= -1.0000000000000002d100 %result%))))

;; REVIEW: CCL? ECL?
#+sbcl
(def-test special-numbers (:suite js-tests)
  (with-js-transform-result "NaN" (%result%)
    (is (sb-ext:float-nan-p %result%)))
  (with-js-transform-result "Infinity" (%result%)
    (is (sb-ext:float-infinity-p %result%))
    (is (equal 1.0d0 (float-sign %result% 1.0))))
  (with-js-transform-result "-Infinity" (%result%)
    (is (sb-ext:float-infinity-p %result%))
    (is (equal -1.0d0 (float-sign %result% 1.0)))))

(def-test fractional-number (:suite js-tests)
  (with-js-transform-result "5.3" (%result%)
    (is (= 5.3d0 %result%))))

(def-test periodic-number (:suite js-tests)
  (with-js-transform-result "var num = 5/3; num" (%result%)
    (is (equal 1.6666666666666667d0 %result%))))

;;; Strings

(def-test simple-string (:suite js-tests)
  (with-js-transform-result "\"hello\"" (%result%)
    (is (equal "hello" %result%))))

(def-test escaped-string (:suite js-tests)
  (with-js-transform-result "\"hello\\nthere\"" (%result%)
    (is (equal "hello
there" %result%))))

(def-test templated-string (:suite js-tests)
  (with-js-transform-result "var num = 5; `${num} + ${num} = ${10}`" (%result%)
    (is (equal "5 + 5 = 10" %result%))))

(def-test concatenated-string (:suite js-tests)
  (with-js-transform-result "\"hello \" + \"there!\"" (%result%)
    (is (equal "hello there!" %result%))))

;;; Arrays

(def-test simple-array (:suite js-tests)
  (with-js-transform-result "[1, 2, 3, 4, 5]" (%result%)
    (is (equal (list 1 2 3 4 5) %result%)))
  (with-js-transform-result "[\"h\", \"e\", \"l\", \"l\", \"o\"]" (%result%)
    (is (equal (list "h" "e" "l" "l" "o") %result%)))
  (with-js-transform-result "[true, false, true, true, false]" (%result%)
    (is (equal (list t nil t t nil) %result%))))

(def-test everything-array (:suite js-tests)
  (with-js-transform-result (%result% "[true, false, undefined, null, 100000, \"hello\", {one: 1}]")
    (is (equal (list t nil :undefined :null 100000 "hello" '(("one" . 1))) %result%))))

(def-test everything-array-vector (:suite js-tests)
  (setf webkit:*js-array-type* :vector)
  (with-js-transform-result (%result% "[true, false, undefined, null, 100000, \"hello\", {one: 1}]")
    (is (equalp (vector t nil :undefined :null 100000 "hello" '(("one" . 1))) %result%)))
  (setf webkit:*js-array-type* :list))

;;; Objects

(def-test single-field-object (:suite js-tests)
  (with-js-transform-result (%result% "var obj = {one: 1}; obj")
    (is (equal (list (cons "one"  1)) %result%))))

(def-test long-object (:suite js-tests)
  (with-js-transform-result
      (%result%
       "var obj = {one: 1, two: 2, three: 3, five: 5, ten: 10, googol: Math.pow(10, 100)}; obj")
    (is (equal (list (cons "one"  1)
                     (cons "two" 2)
                     (cons "three" 3)
                     (cons "five" 5)
                     (cons "ten" 10)
                     (cons "googol" 1.0000000000000002d100))
               %result%))))

(def-test everything-object (:suite js-tests)
  (with-js-transform-result
      (%result%
       "var obj = {one: 1, nul: null, undef: undefined, googol: Math.pow(10, 100),
nil: false, t: true,
o: {one: 1, two: 2, three: 3, five: 5, ten: 10, googol: -Math.pow(10, 100)},
arr: [true, false, undefined, null, 100000, \"hello\", {one: 1}]}; obj")
    (is (equal `(("one" . 1)
                 ("nul" . :null)
                 ("undef" . :undefined)
                 ("googol" . 1.0000000000000002d100)
                 ("nil") ;; Maybe use non-dotted alists instead of this?
                 ("t" . t)
                 ("o" . (("one" . 1)
                         ("two" . 2)
                         ("three" . 3)
                         ("five" . 5)
                         ("ten" . 10)
                         ("googol" . -1.0000000000000002d100)))
                 ("arr" . (t nil :undefined :null 100000 "hello" (("one" . 1)))))
               %result%))))

(def-test everything-object-hash (:suite js-tests)
  (setf webkit:*js-object-type* :hash-table)
  (with-js-transform-result
      (%result%
       "var obj = {one: 1, nul: null, undef: undefined, googol: Math.pow(10, 100),
nil: false, t: true,
o: {one: 1, two: 2, three: 3, five: 5, ten: 10, googol: -Math.pow(10, 100)},
arr: [true, false, undefined, null, 100000, \"hello\", {one: 1}]}; obj")
    (is (hash-table-p %result%))
    (is (equal 8 (hash-table-count %result%)))
    (is (equal 1 (gethash "one" %result%)))
    (is (eq :null (gethash "nul" %result%)))
    (is (eq :undefined (gethash "undef" %result%)))
    (is (equal 1.0000000000000002d100 (gethash "googol" %result%)))
    (is (eq nil (gethash "nil" %result%)))
    (is (eq t (gethash "t" %result%)))
    (is (equalp (let ((alist '(("one" . 1)
                               ("two" . 2)
                               ("three" . 3)
                               ("five" . 5)
                               ("ten" . 10)
                               ("googol" . -1.0000000000000002d100)))
                      (ht (make-hash-table :test 'equal)))
                  (dolist (pair alist)
                    (setf (gethash (first pair) ht) (rest pair)))
                  ht)
                (gethash "o" %result%)))
    (is (equalp `(t nil :undefined :null 100000 "hello"
                    ,(let ((ht (make-hash-table :test 'equal)))
                       (setf (gethash "one" ht) 1)
                       ht))
                (gethash "arr" %result%))))
  (setf webkit:*js-object-type* :alist))

(def-test everything-object-plist (:suite js-tests)
  (setf webkit:*js-object-type* :plist)
  (with-js-transform-result
      (%result%
       "var obj = {one: 1, nul: null, undef: undefined, googol: Math.pow(10, 100),
nil: false, t: true,
o: {one: 1, two: 2, three: 3, five: 5, ten: 10, googol: -Math.pow(10, 100)},
arr: [true, false, undefined, null, 100000, \"hello\", {one: 1}]}; obj")
    (is (equal `(:one 1
                 :nul :null
                 :undef :undefined
                 :googol 1.0000000000000002d100
                 :nil nil
                 :t t
                 :o (:one 1
                     :two 2
                     :three 3
                     :five 5
                     :ten 10
                     :googol -1.0000000000000002d100)
                 :arr (t nil :undefined :null 100000 "hello" (:one 1)))
               %result%)))
  (setf webkit:*js-object-type* :alist))

(run! 'webkit-tests)

(gtk:join-gtk-main)
