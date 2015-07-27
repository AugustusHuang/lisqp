;;;; The MIT License (MIT)

;;;; Copyright (c) 2015 Huang Xuxing

;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

;;;; General arithmetic functions for emulator
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

(defun binary-q-+ (number1 number2)
  "Binary general helper function of quantum addition."
  (declare (type integer number1 number2))
  (binary-q-+-cdkm number1 number2))

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
