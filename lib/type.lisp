;;;; Generic useful type information
;;;; Author: Augustus Huang
;;;; Date: May 22, 2015

(in-package :general-utilities)

(deftype uint () '(integer 0 *))

(deftype amplitude () '(real 0 1))

(deftype angle () '(real #.pi #.(- pi)))

(deftype matrix (&optional type x y)
  `(array ,type (,x ,y)))

(deftype square-matrix (&optional type x)
  `(array ,type (,x ,x)))

(defstruct quantum-register
  "Quantum register, with 'width' qubits and 'l0-norm' non-zero basis states."
  (width 0 :type uint)
  (l0-norm 0 :type uint)
  (amplitude #(1) :type (vector amplitude))
  (pure-states #(0) :type (vector uint)))

(defun unitary-matrix-p (m)
  "Predicate of unitary matrix."
  (declare (type square-matrix m))
  )

(defun identity-matrix-p (m)
  "Predicate of identity matrix."
;;  (declare (type square-matrix m))
  (let ((row (array-dimension m 0)))
    (loop for i from 0 to (- row 1) do
	 (if (/= (aref m i i) 1)
	     (return-from identity-matrix-p nil)))
    t))

(defun trace-zero-p (m)
  "Predicate of zero trace matrix."
  (declare (type square-matrix m))
  (let ((row (array-dimension m 0))
	(sum 0))
    (loop for i from 0 to (- row 1) do
	 (incf sum (aref m i i)))
    (if (= 0 sum)
	t
	nil)))

(defun rank-full-p (m)
  "Predicate of full-rank matrix."
  (declare (type matrix m))
  )