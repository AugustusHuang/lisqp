;;;; General utilities of qubits and quantum registers
;;;; Author: Augustus Huang
;;;; Date: May 25, 2015

(in-package :cl-quantum)

;;; APIs
(defun init-from-file ()
  "Initiate a quantum gate circuit from file."
  )

(defun output-to-file ()
  "Output a quantum gate circuit to file."
  )

(defun print-quantum-register (qreg)
  "Print out a quantum register's content."
  (declare (type quantum-register qreg))
  )

(defun kronecker-quantum-register-2 (qreg1 qreg2)
  "Helper function of the kronecker product of two quantum registers."
  (declare (type quantum-register qreg1 qreg2))
  (let (())))

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
	 
