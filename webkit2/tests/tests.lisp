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

(defvar *view* nil "A testing view.")

(defvar *googol*
  #+sbcl
  1.0000000000000002d100
  #+ecl
  1.l100
  #+ccl
  1.0D+100)

(defmacro with-js-transform-result (js-string (var &optional (jsc-var (gensym))
                                                     (context-var (gensym)))
                                    &body body)
  `(let* ((,context-var (webkit:jsc-context-new))
          (,jsc-var (webkit:jsc-context-evaluate ,context-var ,js-string -1))
          (,var (webkit:jsc-value-to-lisp ,jsc-var)))
     (declare (ignorable ,var ,jsc-var ,context-var))
     ,@body))

;;; General tests

(def-test json-values (:suite js-tests)
  (with-js-transform-result "null"
      (%result% %jsc-result%)
    (is (equal "null" (webkit:jsc-value-to-json %jsc-result% 0))))
  (with-js-transform-result "true"
      (%result% %jsc-result%)
    (is (equal "true" (webkit:jsc-value-to-json %jsc-result% 0))))
  (with-js-transform-result "false"
      (%result% %jsc-result%)
    (is (equal "false" (webkit:jsc-value-to-json %jsc-result% 0))))
  (with-js-transform-result "[1, 2, 3]"
      (%result% %jsc-result%)
    (is (equalp "[1,2,3]" (webkit:jsc-value-to-json %jsc-result% 0))))
  (with-js-transform-result "var obj = {num: 1.3}; obj"
      (%result% %jsc-result%)
    (is (equalp "{\"num\":1.3}" (webkit:jsc-value-to-json %jsc-result% 0))))
  (with-js-transform-result "var obj = {arr: [1, 2, 3], num: 1.3, str: \"hello\", obj: {field: null}}; obj"
      (%result% %jsc-result%)
    (is (equalp "{\"arr\":[1,2,3],\"num\":1.3,\"str\":\"hello\",\"obj\":{\"field\":null}}"
                (webkit:jsc-value-to-json %jsc-result% 0)))))

(def-test back-and-forth (:suite js-tests)
  (with-js-transform-result "var obj = {arr: [1, 2, 3], num: 1.3, str: \"hello\", obj: {field: null}}; obj"
      (%result% %jsc-result% %context%)
    (is (equalp (webkit::jsc-value-to-lisp (webkit::lisp-to-jsc-value %result% %context%))
                %result%))))

;;; Literal types

(def-test undefined (:suite js-tests)
  (with-js-transform-result "undefined" (%result%)
    (is (eq :undefined %result%))))

(def-test null (:suite js-tests)
  (with-js-transform-result "null" (%result%)
    (is (eq :null %result%))))

(def-test null-as-nil (:suite js-tests)
  (setf webkit::*js-null-value* nil)
  (unwind-protect
       (with-js-transform-result "null" (%result%)
         (is (eq nil %result%)))
    (setf webkit::*js-null-value* :null)))

(def-test false (:suite js-tests)
  (with-js-transform-result "false" (%result%)
    (is (eq nil %result%))))

(def-test false-as-keyword (:suite js-tests)
  (setf webkit::*js-false-value* :false)
  (unwind-protect
       (with-js-transform-result "false" (%result%)
         (is (eq :false %result%)))
    (setf webkit::*js-false-value* nil)))

(def-test true (:suite js-tests)
  (with-js-transform-result "true" (%result%)
    (is (eq t %result%))))

(def-test true-as-keyword (:suite js-tests)
  (setf webkit::*js-true-value* :true)
  (unwind-protect
       (with-js-transform-result "true" (%result%)
         (is (eq :true %result%)))
    (setf webkit::*js-true-value* t)))

;;; Numbers

(def-test integers (:suite js-tests)
  (with-js-transform-result "0" (%result%)
    (is (= 0 %result%)))
  (with-js-transform-result "8" (%result%)
    (is (= 8 %result%)))
  (with-js-transform-result "-6" (%result%)
    (is (= -6 %result%)))
  (with-js-transform-result "Math.pow(10, 100)" (%result%)
    (is (= *googol* %result%)))
  (with-js-transform-result "-Math.pow(10, 100)" (%result%)
    (is (= (- *googol*) %result%))))

(def-test special-numbers (:suite js-tests)
  (with-js-transform-result "NaN" (%result%)
    (is (float-features:float-nan-p %result%)))
  (with-js-transform-result "Infinity" (%result%)
    (is (float-features:float-infinity-p %result%))
    (is (equal 1 (truncate (float-sign %result% 1.0)))))
  (with-js-transform-result "-Infinity" (%result%)
    (is (float-features:float-infinity-p %result%))
    (is (equal -1 (truncate (float-sign %result% 1.0))))))

(def-test fractional-number (:suite js-tests)
  (with-js-transform-result "5.3" (%result%)
    (is (= 5.3 (coerce %result% 'single-float))))
  (with-js-transform-result "-34435.3044" (%result%)
    (is (= -34435.3044 (coerce %result% 'single-float)))))

(def-test periodic-number (:suite js-tests)
  (with-js-transform-result "var num = 5/3; num" (%result%)
    (is (equal 1.6666666666666667 (coerce %result% 'single-float)))))

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
  (setf webkit:*js-object-type* :alist)
  (unwind-protect
       (with-js-transform-result "[true, false, undefined, null, 100000, \"hello\", {one: 1}]" (%result%)
         (is (equal (list t nil :undefined :null 100000 "hello" '(("one" 1))) %result%)))
    (setf webkit:*js-object-type* :hash-table)))

(def-test everything-array-vector (:suite js-tests)
  (setf webkit:*js-array-type* :vector
        webkit:*js-object-type* :alist)
  (unwind-protect
       (with-js-transform-result "[true, false, undefined, null, 100000, \"hello\", {one: 1}]" (%result%)
         (is (equalp (vector t nil :undefined :null 100000 "hello" '(("one" 1))) %result%)))
    (setf webkit:*js-array-type* :list
          webkit:*js-object-type* :hash-table)))

;;; Objects

(def-test single-field-object (:suite js-tests)
  (setf webkit:*js-object-type* :alist)
  (unwind-protect
       (with-js-transform-result "var obj = {one: 1}; obj" (%result%)
         (is (equal '(("one"  1)) %result%)))
    (setf webkit:*js-object-type* :hash-table)))

(def-test long-object (:suite js-tests)
  (setf webkit:*js-object-type* :alist)
  (unwind-protect
       (with-js-transform-result
           "var obj = {one: 1, two: 2, three: 3, five: 5, ten: 10, googol: Math.pow(10, 100)}; obj"
           (%result%)
         (is (equal `(("one"  1)
                      ("two" 2)
                      ("three" 3)
                      ("five" 5)
                      ("ten" 10)
                      ("googol" ,*googol*))
                    %result%)))
    (setf webkit:*js-object-type* :hash-table)))

(def-test everything-object (:suite js-tests)
  (setf webkit:*js-object-type* :alist)
  (unwind-protect
       (with-js-transform-result
           "var obj = {one: 1, nul: null, undef: undefined, googol: Math.pow(10, 100),
nil: false, t: true,
o: {one: 1, two: 2, three: 3, five: 5, ten: 10, googol: -Math.pow(10, 100)},
arr: [true, false, undefined, null, 100000, \"hello\", {one: 1}]}; obj"
           (%result%)
         (is (equal `(("one" 1)
                      ("nul" :null)
                      ("undef" :undefined)
                      ("googol" ,*googol*)
                      ("nil" nil) ;; Maybe use non-dotted alists instead of this?
                      ("t" t)
                      ("o" (("one" 1)
                            ("two" 2)
                            ("three" 3)
                            ("five" 5)
                            ("ten" 10)
                            ("googol" ,(- *googol*))))
                      ("arr" (t nil :undefined :null 100000 "hello" (("one" 1)))))
                    %result%)))
    (setf webkit:*js-object-type* :hash-table)))

(def-test everything-object-hash (:suite js-tests)
  (with-js-transform-result
      "var obj = {one: 1, nul: null, undef: undefined, googol: Math.pow(10, 100),
nil: false, t: true,
o: {one: 1, two: 2, three: 3, five: 5, ten: 10, googol: -Math.pow(10, 100)},
arr: [true, false, undefined, null, 100000, \"hello\", {one: 1}]}; obj"
      (%result%)
    (is (hash-table-p %result%))
    (is (equal 8 (hash-table-count %result%)))
    (is (equal 1 (gethash "one" %result%)))
    (is (eq :null (gethash "nul" %result%)))
    (is (eq :undefined (gethash "undef" %result%)))
    (is (equal *googol* (gethash "googol" %result%)))
    (is (eq nil (gethash "nil" %result%)))
    (is (eq t (gethash "t" %result%)))
    (is (equalp (let ((alist `(("one" 1)
                               ("two" 2)
                               ("three" 3)
                               ("five" 5)
                               ("ten" 10)
                               ("googol" ,(- *googol*))))
                      (ht (make-hash-table :test 'equal)))
                  (dolist (pair alist)
                    (setf (gethash (first pair) ht) (second pair)))
                  ht)
                (gethash "o" %result%)))
    (is (equalp `(t nil :undefined :null 100000 "hello"
                    ,(let ((ht (make-hash-table :test 'equal)))
                       (setf (gethash "one" ht) 1)
                       ht))
                (gethash "arr" %result%)))))

(def-test everything-object-plist (:suite js-tests)
  (setf webkit:*js-object-type* :plist)
  (unwind-protect
       (with-js-transform-result
           "var obj = {one: 1, nul: null, undef: undefined, googol: Math.pow(10, 100),
nil: false, t: true,
o: {one: 1, two: 2, three: 3, five: 5, ten: 10, googol: -Math.pow(10, 100)},
arr: [true, false, undefined, null, 100000, \"hello\", {one: 1}]}; obj"
           (%result%)
         (is (equal `(:one 1
                      :nul :null
                      :undef :undefined
                      :googol ,*googol*
                      :nil nil
                      :t t
                      :o (:one 1
                          :two 2
                          :three 3
                          :five 5
                          :ten 10
                          :googol ,(- *googol*))
                      :arr (t nil :undefined :null 100000 "hello" (:one 1)))
                    %result%)))
    (setf webkit:*js-object-type* :hash-table)))

;;; Functions

(def-test zero-fn (:suite js-tests)
  (let ((zero-fn (webkit:make-jsc-function (*view*) () 0)))
    (is (zerop (funcall zero-fn)))
    (is (functionp zero-fn))))

(def-test simple-function (:suite js-tests)
  (let ((add-one (webkit:make-jsc-function (*view*) (number)
                   (1+ number))))
    (is (= 6 (funcall add-one 5)))
    (is (functionp add-one))))

(def-test five-arg-function (:suite js-tests)
  (let ((five-args (webkit:make-jsc-function (*view*) (one two three four five)
                     (+ one (expt two three) (* four five)))))
    (is (= 9150 (funcall five-args 10 20 3 57 20)))
    (is (functionp five-args))))

(def-test list-function (:suite js-tests)
  (let ((js-append (webkit:make-jsc-function (*view*) (l1 l2)
                     (append l1 l2))))
    (is (functionp js-append))
    (is (equalp '(1 2 3) (funcall js-append '(1 2 3) nil)))
    (is (equalp '(1 2 3 4 5) (funcall js-append '(1 2 3) '(4 5))))))

(def-test object-function (:suite js-tests)
  (let ((hash (make-hash-table :test 'equal))
        (hash-to-alist (webkit:make-jsc-function (*view*) (hash)
                         (let ((alist '()))
                           (maphash (lambda (key value)
                                      (push (list key value) alist))
                                    hash)
                           alist))))
    (setf (gethash "a" hash) 1)
    (setf (gethash "b" hash) '(1 2 3))
    (setf (gethash "c" hash) "hello")
    (is (functionp hash-to-alist))
    (is (equalp '(("a" 1) ("b" (1 2 3)) ("c" "hello")) (funcall hash-to-alist hash)))))

;;; Classes

(def-test empty-class (:suite js-tests)
  (let* ((context (webkit:jsc-context-new))
         (empty-class (webkit:make-jsc-class (nil "Empty" context) () ())))
    (is (equal 'webkit:jsc-class (type-of empty-class)))
    (is (equal "Empty" (webkit:jsc-class-get-name empty-class)))
    (is (null (webkit:jsc-class-get-parent empty-class)))
    (let ((empty (webkit:jsc-context-evaluate context "var e = new Empty(); e" -1)))
      (is (equal 'webkit:jsc-value (type-of empty)))
      (is (webkit:jsc-value-is-object empty))
      (is (equal t (webkit:jsc-value-object-is-instance-of empty "Empty"))))))

(def-test single-property-class (:suite js-tests)
  (let* ((context (webkit:jsc-context-new))
         (five-new-value 0)
         (single-class (webkit:make-jsc-class (nil "Single" context) ()
                         ((five :reader (lambda (instance)
                                          (declare (ignore instance))
                                          five-new-value)
                                :writer (lambda (instance value)
                                          (declare (ignore instance))
                                          (setf five-new-value value))))
                         (:constructor (lambda (class)
                                         (setf five-new-value 5)
                                         (g:pointer
                                          (webkit:jsc-value-new-object
                                           context (cffi:null-pointer) class)))))))
    (is (equal 'webkit:jsc-class (type-of single-class)))
    (is (equal "Single" (webkit:jsc-class-get-name single-class)))
    (is (null (webkit:jsc-class-get-parent single-class)))
    (let ((single (webkit:jsc-context-evaluate context "var s = new Single(); s" -1)))
      (is (equal 'webkit:jsc-value (type-of single)))
      (is (webkit:jsc-value-is-object single))
      (is (equal t (webkit:jsc-value-object-is-instance-of single "Single")))
      (is (equal 5 (webkit:jsc-value-to-lisp (webkit:jsc-context-evaluate context "s.five" -1))))
      (progn
        (webkit:jsc-context-evaluate context "s.five = 10" -1)
        (is (= 10 five-new-value))))))

(def-test methods-class (:suite js-tests)
  (let* ((context (webkit:jsc-context-new))
         (sum-slot 0)
         (methods-class (webkit:make-jsc-class (nil "Methods" context) ()
                          ((sum :reader (lambda (instance)
                                          (declare (ignore instance))
                                          sum-slot)
                                :writer (lambda (instance value)
                                          (declare (ignore instance))
                                          (setf sum-slot value)))))))
    (webkit:make-jsc-method add ((object methods-class) arg1 arg2)
      (+ arg1 arg2))
    (webkit:make-jsc-method add-with-sum ((object methods-class) arg1 arg2)
      ;; TODO: This hints at the necessity of jsc-slot-value.
      (incf sum-slot (+ arg1 arg2)))
    (is (equal 'webkit:jsc-class (type-of methods-class)))
    (is (equal "Methods" (webkit:jsc-class-get-name methods-class)))
    (is (null (webkit:jsc-class-get-parent methods-class)))
    (let ((methods (webkit:jsc-context-evaluate context "var m = new Methods(); m" -1)))
      (is (equal 'webkit:jsc-value (type-of methods)))
      (is (webkit:jsc-value-is-object methods))
      (is (equal t (webkit:jsc-value-object-is-instance-of methods "Methods")))
      (is (= 0 sum-slot))
      (is (= 5 (webkit:jsc-value-to-lisp (webkit:jsc-context-evaluate context "m.add(1, 4)" -1))))
      (when (webkit:jsc-context-evaluate context "m.sum = 20;" -1)
        (is (= 20 sum-slot)))
      (when (webkit:jsc-context-evaluate context "m.addWithSum(1, 201)" -1)
        (is (= 222 sum-slot))))))

;;; Setup function

(defun run-tests ()
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
      (setf *view* view)))
  (run! 'webkit-tests)
  (gtk:join-gtk-main)
  (gtk:gtk-main-quit))
