;;;; The MIT License (MIT)

;;;; Copyright (c) 2015 Huang Xuxing

;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

;;;; LISQP interpreter top-level routines and utilities.
;;;; Date: May 22, 2015

(in-package :cl-lisqp)

;;; APIs
;;; When running with default arguments, this interpreter will act like a
;;; normal LISP interpreter, with ordinary read-eval-print loop, when
;;; eval is replaced by q-eval, it will be turned into a lisqp repl.
(defun top-level (&key (prompt "=> ") (read #'read)
		  (eval #'eval) (print #'print))
  "lisqp's repl, act like an ordinary lisp repl or lisqp repl depends on arguments."
  (with-simple-restart
      (abort "Exit lisqp repl.")
    (loop
	 (with-simple-restart
	     (abort "Return to lisqp repl.")
	   (format t "~&~A" prompt)
	   (funcall print (funcall eval (funcall read)))))))
