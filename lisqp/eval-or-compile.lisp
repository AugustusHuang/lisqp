;;;; Lisqp eval-or-compile routines
;;;; Author: Augustus Huang
;;;; Date: May 29, 2015

(in-package :cl-lisqp)

;;; APIs
(defun eval ()
  "Evaluate routine used by interpreter.")

(defun compile ()
  "Compile routine direct translate forms into emulator opcodes.")