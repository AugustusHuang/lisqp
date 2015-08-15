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

;;;; Grover algorithm

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
