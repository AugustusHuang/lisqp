;;;; General arithmetic functions for emulator
;;;; Author: Augustus Huang
;;;; Date: May 25, 2015

(in-package :cl-quantum-emulator)

(defun binary-q-+-cdkm (number1 number2)
  "Binary helper function of quantum addition based on CDKM model."
  (declare (type integer number1 number2))
  )

(defun binary-q-+-vbe (number1 number2)
  "Binary helper function of quantum addition based on VBE model."
  (declare (type integer number1 number2))
  )

(defun binary-q-+-draper (number1 number2)
  "Binary helper function of quantum addition based on Draper's model."
  (declare (type integer number1 number2))
  )

(defun binary-q-+ (number1 number2)
  "Binary general helper function of quantum addition."
  (declare (type integer number1 number2))
  )

(defun binary-q-+-mod (number1 number2 mod)
  "Binary helper function of add-mod algorithm."
  )

(defun binary-q-*-mod (number1 number2 mod)
  "Binary helper function of mul-mod algorithm."
  )

(defun multiply ())

(defun multiply-mod (n mod control ancilla qreg)
  "Multiply and take modulus."
  (declare (type fixnum control ancilla)
	   (type quantum-register qreg))
  )

(defun q-expt-mod (n a width ancilla qreg)
  "Quantum exponent and modulus algorithm."
  (declare (type fixnum width ancilla)
	   (type quantum-register qreg))
  (let ((modulus (mod a n)))
    (pauli-x (+ 2 (* 2 ancilla)) qreg)
    (loop for i from 1 to width do
	 (setf modulus (mod a n))
	 (loop for j from 1 to (1- i) do
	      (setf modulus (* modulus modulus))
	      (setf modulus (mod modulus n)))
	 (multiply-mod n modulus (+ 1 i (* 3 ancilla)) ancilla qreg)
       ;; And return?
	 )))

;;; APIs
(defun q-+ (&rest numbers)
  "General quantum add function."
  (if (null numbers)
      0
      (reduce #'binary-q-+ numbers)))

(defun q-+-mod (mod &rest numbers)
  "General quantum add-mod function."
  (if (null numbers)
      0
      (reduce #'binary-q-+-mod numbers)))

(defun q-*-mod (mod &rest numbers)
  "General quantum mul-mod function."
  (if (null numbers)
      1
      (reduce #'binary-q-*-mod numbers)))
