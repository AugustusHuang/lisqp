;;;; Quantum gates
;;;; Author: Augustus Huang
;;;; Date: May 25, 2015

(in-package :cl-quantum)

(defun apply-gate (operator target qreg)
  "Generic gate function."
  (declare (type square-matrix operator)
	   (type integer target)
	   (type quantum-register qreg))
  )

;;; APIs
(defun c-not (control target qreg)
  "Controlled not gate."
  (declare (type integer control target)
	   (type quantum-register qreg))
  )

(defun c-not-ec (control target qreg)
  "Controlled not gate with error correction."
  (declare (type integer control target)
	   (type quantum-register qreg))
  )

(defun hadamard (target qreg)
  "Hadamard gate."
  (declare (type integer target)
	   (type quantum-register qreg))
  )

(defun toffoli (control1 control2 target qreg)
  "Toffoli gate."
  (declare (type integer control1 control2 target)
	   (type quantum-register qreg))
  )

(defun toffoli-ec (control1 control2 target qreg)
  "Toffoli gate with error correction."
  (declare (type integer control1 control2 target)
	   (type quantum-register qreg))
  )

(defun swap ()
  "Swapping gate."
  )

(defun phase ()
  "Phase gate."
  )

(defun pauli-x (target qreg)
  "Pauli x gate."
  (declare (type integer target)
	   (type quantum-register qreg))
  )

(defun pauli-x-ec (target qreg)
  "Pauli x gate with error correction."
  (declare (type integer target)
	   (type quantum-register qreg))
  )

(defun pauli-y (target qreg)
  "Pauli y gate."
  (declare (type integer target)
	   (type quantum-register qreg))
  )

(defun pauli-z (target qreg)
  "Pauli z gate."
  (declare (type integer target)
	   (type quantum-register qreg))
  )

(defun pi/8 (target qreg)
  "Pi/8 gate."
  (declare (type integer target)
	   (type quantum-register qreg))
  )

(defun c-pauli-z (control target qreg)
  "Controlled pauli z gate."
  (declare (type integer control target)
	   (type quantum-register qreg))
  )

(defun c-phase (control target qreg)
  "Controlled phase gate."
  (declare (type integer control target)
	   (type quantum-register qreg))
  )

(defun inverse-c-phase (control target qreg)
  "Inverse controlled phase gate."
  (declare (type integer control target)
	   (type quantum-register qreg))
  )

(defun fredkin ()
  "Fredkin gate."
  )