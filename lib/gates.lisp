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
  (let ((current-level (get-decoherence-level))
	(current-width (get-ec-width)))
    (set-decoherence-level 0)
    (set-ec-width 0)
    (c-not control target qreg)
    (c-not (+ control current-width) (+ target current-width) qreg)
    (set-decoherence-level current-level)
    (c-not (+ control (* 2 current-width)) (+ target (* 2 current-width)) qreg)
    (set-ec-width current-width)))

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
  (let* ((current-level (get-decoherence-level))
	 (current-width (get-ec-width))
	 (mask (+ (ash 1 target)
		  (ash 1 (+ target current-width))
		  (ash 1 (+ target (* 2 current-width)))))
	 (l0 (get-q-l0-norm qreg))
	 (states (get-q-pure-states qreg)))
    (loop for i from 0 to (- l0 1) do
	 (let ((c1 0)
	       (c2 0))
	   (if (logtest (svref states i) (ash 1 control1))
	       (setf c1 1))
	   (if (logtest (svref states i) (ash 1 (+ control1 current-width)))
	       (setf c1 (boole boole-xor c1 1)))
	   (if (logtest (svref states i) (ash 1 (+ control1 (* 2 current-width))))
	       (setf c1 (boole boole-xor c1 1)))
	   (if (logtest (svref states i) (ash 1 control2))
	       (setf c2 1))
	   (if (logtest (svref states i) (ash 1 (+ control2 current-width)))
	       (setf c2 (boole boole-xor c2 1)))
	   (if (logtest (svref states i) (ash 1 (+ control2 (* 2 current-width))))
	       (setf c2 (boole boole-xor c2 1)))
	   (if (= c1 c2 1)
	       (setf (svref states i) (boole boole-xor (svref states i) mask)))))
    (decohere qreg)))

(defun swap (control target qreg)
  "Swapping gate."
  (declare (type integer control target)
	   (type quantum-register qreg))
  )

(defun phase (target qreg)
  "Phase gate."
  (declare (type integer target)
	   (type quantum-register qreg))
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
  (let ((current-level (get-decoherence-level))
	(current-width (get-ec-width)))
    (set-decoherence-level 0)
    (set-ec-width 0)
    (pauli-x target qreg)
    (pauli-x (+ target current-width) qreg)
    (set-decoherence-level current-level)
    (pauli-x (+ target (* 2 current-width)) qreg)
    (set-ec-width current-width)))

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

(defun fredkin (control1 control2 target qreg)
  "Fredkin gate."
  (declare (type integer control1 control2 target)
	   (type quantum-register qreg))
  )

(defun measure (target qreg)
  "Measure 'target' qubits in the quantum-register."
  (declare (type integer target)
	   (type quantum-register qreg))
  )