;;;; Decoherence of quantum states
;;;; Author: Augustus Huang
;;;; Date: May 25, 2015

(in-package :cl-quantum)

;;; Decoherence level, 0 means disabled.
(defparameter *decoherence-level* 0)

(declaim (inline get-decoherence-level))
(defun get-decoherence-level ()
  "Get current decoherence level."
  *decoherence-level*)

(declaim (inline set-decoherence-level))
(defun set-decoherence-level (level)
  "Set current decoherence level to level."
  (setf *decoherence-level* level))

(defun decohere ()
  "Decohere."
  )