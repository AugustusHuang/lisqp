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

;;;; Quantum Fourier Transformation

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
