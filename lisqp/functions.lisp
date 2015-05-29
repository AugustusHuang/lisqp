;;;; Lisqp intepreter functions
;;;; Author: Augustus Huang
;;;; Date: May 27, 2015

(in-package :cl-lisqp)

;;; All quantum functions will begin with '%'.
;;; I choose this because it looks like <bra|ket>!
;;; Likely, all classic functions runs on the host will begin with '@'.
(defun %defcategory ()
  "Define a generic category.")

(defun %defmonad ()
  "Define a generic monad.")

(defun %swap ()
  "Swap two numbers.")