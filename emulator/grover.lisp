;;;; Grover algorithm
;;;; Author: Augustus Huang
;;;; Date: June 18, 2015

(in-package :cl-quantum)

(defun grover (target qreg)
  "Quantum grover algorithm."
  (declare (type fixnum target)
	   (type quantum-register qreg))
  (let ((width (get-q-width qreg)))
    (oracle target qreg)
    (loop for i from 0 to (1- width) do
	 (hadamard i qreg))
    (inversion qreg)
    (loop for i from 0 to (1- width) do
	 (hadamard i qreg))))

(defun oracle (target qreg)
  "General oracle definition."
  (declare (type fixnum target)
	   (type quantum-register qreg))
  (let ((width (get-q-width qreg)))
    (loop for i from 0 to (1- width) do
	 (if (not (logtest target (ash 1 i)))
	     (pauli-x i qreg)))
    (toffoli 0 1 (1+ width) qreg)
    (loop for i from 1 to (1- width) do
	 (toffoli i (+ width i) (+ width 1 i) qreg))
    (c-not (+ width i) width qreg)
    (loop for i from (1- width) downto 0 do
	 (toffoli i (+ width 1) (+ width i 1) qreg))
    (toffoli 0 1 (1+ width) qreg)
    (loop for i from 0 to (1- width) do
	 (if (not (logtest target (ash 1 i)))
	     (pauli-x i qreg)))))

(defun inversion (qreg)
  "Inversion about mean operator."
  (declare (type quantum-register qreg))
  (let ((width (get-q-width qreg)))
    (loop for i from 0 to (1- width) do
	 (pauli-x i qreg))
    (hadamard (1- width) qreg)
    (if (= width 3)
	(toffoli 0 1 2 qreg)
	(progn
	  (toffoli 0 1 (1+ width) qreg)
	  (loop for i from 1 to (1- width) do
	       (toffoli i (+ width i) (+ width i 1) qreg))
	  (c-not (+ i width) (1- width) qreg)
	  (loop for i from (2- width) downto 0 do
	       (toffoli i (+ width i) (+ width i 1) qreg))
	  (toffoli 0 1 (1+ width) qreg)))
    (hadamard (1- width) qreg)
    (loop for i from 0 to (1- width) do
	 (pauli-x i qreg))))
