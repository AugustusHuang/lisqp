;;;; Decoherence of quantum states
;;;; Author: Augustus Huang
;;;; Date: May 25, 2015

(in-package :cl-quantum)

;;; Decoherence level, 0 means disabled.
(defparameter *decoherence-level* 0)

(declaim (inline get-decoherence-level))
(defun get-decoherence-level ()
  "Get current decoherence level, 0 means decoherence not enabled."
  *decoherence-level*)

(declaim (inline set-decoherence-level))
(defun set-decoherence-level (level)
  "Set current decoherence level to level, above 1 means decoherence enabled."
  (setf *decoherence-level* level))

(defun decohere (qreg)
  "Decohere."
  (declare (type quantum-register qreg)))