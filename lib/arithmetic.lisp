;;;; The MIT License (MIT)

;;;; Copyright (c) 2015 Huang Xuxing

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :cl-quantum)

(defun expt-mod (n a width ancilla qreg)
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

(defun *-mod (n mod control ancilla qreg)
  )

(defun +-mod (n mod control ancilla qreg)
  )

(defun +-cdkm ()
  )
