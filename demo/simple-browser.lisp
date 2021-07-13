;;; simple-browser.lisp --- webkit browser demo

;; This file is part of cl-webkit.
;;
;; cl-webkit is free software; you can redistribute it and/or modify
;; it under the terms of the MIT license.
;; See `COPYING' in the source distribution for details.

;;; Commentary:
;;
;; The simplest example: a WebKit frame which loads and renders a single website.

;;; Code:

(in-package :cl-user)

(require :asdf)
(asdf:load-systems :cl-cffi-gtk :cl-webkit2)

(defun simple-browser-main ()
  "A single-window browser with no keyboard or mouse input.
Loads and renders a single web page."
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
      (gtk:gtk-widget-show-all win))))

(defun styled-browser-main ()
  "A red-text version of `simple-browser-main'."
  (gtk:within-main-loop
    (let* ((win (make-instance 'gtk:gtk-window))
           (manager (make-instance 'webkit:webkit-website-data-manager
                                   :base-data-directory "testing-data-manager"))
           (context (make-instance 'webkit:webkit-web-context
                                   :website-data-manager manager))
           (view (make-instance 'webkit:webkit-web-view
                                :web-context context))
           (content-manager (webkit:webkit-web-view-get-user-content-manager view)))
      (webkit:webkit-user-content-manager-add-style-sheet
       content-manager
       (webkit:webkit-user-style-sheet-new
        "body { color: #F00 !important; background-color: #000 !important; };"
        :webkit-user-content-inject-all-frames
        :webkit-user-style-level-author
        (cffi:null-pointer)
        (cffi:null-pointer)))
      (gobject:g-signal-connect win "destroy"
                                #'(lambda (widget)
                                    (declare (ignore widget))
                                    (gtk:leave-gtk-main)))
      (gtk:gtk-container-add win view)
      (webkit2:webkit-web-view-load-uri view "http://www.example.com")
      (gtk:gtk-widget-show-all win))))

(defun private-browser-main ()
  "An ephemeral (a.k.a private) mode version of `simple-browser-main'."
  (gtk:within-main-loop
   (let* ((win (make-instance 'gtk:gtk-window))
          (context (webkit:webkit-web-context-new-ephemeral))
          (view (make-instance 'webkit2:webkit-web-view :web-context context)))
     (gobject:g-signal-connect win "destroy"
                               #'(lambda (widget)
                                   (declare (ignore widget))
                                   (gtk:leave-gtk-main)))
     (gtk:gtk-container-add win view)
     (webkit2:webkit-web-view-load-uri view "http://www.example.com")
     (gtk:gtk-widget-show-all win))))

(defun extended-browser-main ()
  "A version of `simple-browser-main' that adds a simple WebKitWebExtension."
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
      (webkit:webkit-web-context-set-web-extensions-directory
       context (namestring
                (merge-pathnames "demo/test-extensions/"
                                 (uiop:pathname-parent-directory-pathname
                                  (asdf:system-source-directory
                                   (asdf:find-system :cl-webkit2))))))
      (gtk:gtk-container-add win view)
      (webkit2:webkit-web-view-load-uri view "http://www.example.com")
      (gtk:gtk-widget-show-all win))))

(defun custom-scheme-browser-main ()
  "A version of `simple-browser-main' that adds a hello: scheme that greets the user."
  (gtk:within-main-loop
    (let* ((win (make-instance 'gtk:gtk-window))
           (context (make-instance 'webkit:webkit-web-context))
           (view (make-instance 'webkit2:webkit-web-view
                                :web-context context)))
      (gobject:g-signal-connect win "destroy"
                                #'(lambda (widget)
                                    (declare (ignore widget))
                                    (gtk:leave-gtk-main)))
      (webkit:webkit-web-context-register-uri-scheme-callback
       context "hello"
       #'(lambda (request)
           (format nil "<html><body><p>Hello, ~:(~a~)!</p></body></html>"
                   (webkit:webkit-uri-scheme-request-get-path request))))
      (gtk:gtk-container-add win view)
      (webkit2:webkit-web-view-load-uri view "hello:stranger")
      (gtk:gtk-widget-show-all win))))

(defun js-transform-browser-main ()
  "A JS transformation-validating variant of `simple-browser-main'."
  (gtk:within-main-loop
    (let* ((win (make-instance 'gtk:gtk-window))
           (context (make-instance 'webkit:webkit-web-context))
           (view (make-instance 'webkit2:webkit-web-view
                                :web-context context)))
      (macrolet ((with-js-transform-tests (view callback &body clauses)
                   `(let ((result '()))
                      ,@(loop for (js-string lisp-value) in clauses
                              for index from 0 to (length clauses)
                              collect
                              `(webkit2:webkit-web-view-evaluate-javascript
                                ,view
                                ,js-string
                                (lambda (object jsc-object)
                                  (declare (ignore jsc-object))
                                  (push (let ((success (equalp ,lisp-value object)))
                                          (list success ,js-string ,lisp-value))
                                        result)
                                  (when (= (length result) ,(length clauses))
                                    (funcall ,callback result))))))))
        (gobject:g-signal-connect win "destroy"
                                  #'(lambda (widget)
                                      (declare (ignore widget))
                                      (gtk:leave-gtk-main)))
        (gtk:gtk-container-add win view)
        (setf webkit:*js-object-type* :alist)
        (with-js-transform-tests view
          (lambda (result)
            (webkit:webkit-web-view-load-html
             view
             (format nil "
<html>
<body>
<h1>Testing Javascript values transformation</h1>
<table>
<tr>
<th>Status</td>
<th>JS value</td>
<th>Lisp value</td>
</tr>
~:{<tr>
<td>~:[X~;✓~]</td>
<td>~S</td>
<td>~S</td>
</tr>~%~}
</table>
</body>
</html>" result)
             (cffi:null-pointer)))
          ("5" 5)
          ("\"hello\"" "hello")
          ("[1, 2, 3, 4, 5]" '(1 2 3 4 5))
          ("[1, null, undefined, \"hello\", 7]" '(1 :null :undefined "hello" 7))
          ("var obj = {hello: 5, how: \"are\", you: null, today: undefined}; obj"
           '(("hello" 5) ("how" "are") ("you" :null) ("today" :undefined))))
        (gtk:gtk-widget-show-all win)))))

(defun test-browser-main (&key private extended styled custom-scheme js-transform)
  (cond
    (private (private-browser-main))
    (extended (extended-browser-main))
    (styled (styled-browser-main))
    (custom-scheme (custom-scheme-browser-main))
    (js-transform (js-transform-browser-main))
    (t (simple-browser-main)))
  (gtk:join-gtk-main))
