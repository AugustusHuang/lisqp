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

;;;; General utilities of qubits and quantum registers

(in-package :cl-quantum)

(defun print-quantum-register (qreg)
  "Print out a quantum register's content."
  (declare (type quantum-register qreg))
  (let ((l0 (get-q-l0-norm qreg))
	(width (get-q-width qreg))
	(states (get-q-pure-states qreg))
	(amps (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- l0) do
	 (let ((amp (aref amps i))
	       (state (aref states i)))
	   (format t "~f+~fi|~f||~d> (|" (realpart amp) (imagpart amp)
		   (complex-norm amp) state)
	   (loop for j from (1- width) downto 0 do
		(if (= 0 (mod j 4))
		    (format t " "))
		(if (> (boole boole-and (ash 1 j) state 0))
		    (format t "1")
		    (format t "0")))
	   (format t ">)~%"))))

;;; Destructive function.
(defun normalize-quantum-register (qreg)
  "Make a quantum register into normalized form."
  (declare (type quantum-register qreg))
  (let ((prob 0)
	(amps (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- (length amps)) do
	 (incf prob (aref amps i)))
    (loop for i from 0 to (1- (length amps)) do
	 (setf (aref amps i) (/ (aref amps i) (sqrt prob))))))

(defun kronecker-quantum-register-2 (qreg1 qreg2)
  "Helper function of the kronecker product of two quantum registers."
  (declare (type quantum-register qreg1 qreg2))
  (let* ((q1-width (get-q-width qreg1))
	 (q2-width (get-q-width qreg2))
	 (width (+ q1-width q2-width))
	 (q1-l0 (get-q-l0-norm qreg1))
	 (q2-l0 (get-q-l0-norm qreg2))
	 (l0-norm (* q1-l0 q2-l0))
	 (amps (make-array l0-norm :element-type 'complex :initial-element 0))
	 (states (make-array l0-norm :element-type 'uint :initial-element 0))
	 (out (make-quantum-register :width width
				     :l0-norm l0-norm
				     :amplitudes amps
				     :pure-states states)))
    (loop for i from 0 to (1- q1-l0) do
	 (loop for j from 0 to (1- q2-l0) do
	      (setf (aref (get-q-pure-states out) (+ j (* i q2-l0)))
		    (boole boole-ior (ash (aref (get-q-pure-states qreg1) i) q2-width)
			   (aref (get-q-pure-states qreg2) i))
		    (aref (get-q-amplitudes out) (+ j (* i q2-l0)))
		    (* (aref (get-q-amplitudes qreg1) i)
		       (aref (get-q-amplitudes qreg2) j)))))
    out))

(defun kronecker-quantum-register (qreg &rest more)
  "Kronecker product of quantum registers."
  (declare (type quantum-register qreg))
  (reduce #'kronecker-quantum-register-2 (cons qreg more)))

;;; These two functions are classical.
(defun qreg-to-vector (qreg)
  "Cast a quantum register into a general vector."
  (declare (type quantum-register qreg))
  (let ((out (make-array (get-q-width qreg) :initial-element 0))
	(states (get-q-pure-states qreg))
	(amps (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- (length states)) do
	 (setf (aref out (aref states i)) (aref amps i)))
    out))

(defun qreg-to-sparse-vector (qreg)
  "Cast a quantum register into a sparse vector."
  (declare (type quantum-register qreg))
  (let* ((vlen (get-q-l0-norm qreg))
	 (out (make-sparse-vector :values (make-array vlen :initial-element 0)
				  :indices (make-array vlen :initial-element 0)
				  :len (get-q-width qreg)))
	 (states (get-q-pure-states qreg))
	 (amps (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- vlen) do
	 (setf (aref (sparse-vector-values out) i)
	       (aref amps i)
	       (aref (sparse-vector-indices out) i)
	       (aref states i)))
    out))
