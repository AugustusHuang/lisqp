;;;; Quantum Fourier Transformation
;;;; Author: Augustus Huang
;;;; Date: May 26, 2015

(in-package :cl-quantum-emulator)

(defun qft (width qreg)
  "Quantum fourier transform routine on a quantum-register."
  (declare (type integer width)
	   (type quantum-register qreg))
  (loop for i from (- width 1) downto 0 do
       (loop for j from (- width 1) above i do
	    (c-phase j i qreg))
       (hadamard i qreg)))

(defun inverse-qft (width qreg)
  "Inverse quantum fourier transform."
  (declare (type integer width)
	   (type quantum-register qreg))
  (loop for i from 0 below width do
       (hadamard i qreg)
       (loop for j from (+ i 1) below width do
	    (inverse-c-phase j i qreg))))