;;;; LISQP interpreter top-level routines and utilities.
;;;; Author: Augustus Huang
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
