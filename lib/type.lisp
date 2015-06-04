;;;; Generic useful type information
;;;; Author: Augustus Huang
;;;; Date: May 22, 2015

(in-package :general-utilities)

(deftype uint ()
  "Unsigned integer."
  '(integer 0 *))

(deftype angle ()
  "Bloch sphere angle."
  '(real #.pi #.(- pi)))

(deftype matrix (&optional type x y)
  "General 2D array."
  `(array ,type (,x ,y)))

(deftype square-matrix (&optional type x)
  "2D array with equal dimensions."
  `(array ,type (,x ,x)))

(defstruct quantum-register
  "Quantum register, with 'width' qubits and 'l0-norm' non-zero basis states."
  (width 0 :type uint)
  (l0-norm 0 :type uint)
  (amplitude #(1) :type (vector complex))
  (pure-states #(0) :type (vector uint)))

(declaim (inline get-q-width get-q-l0-norm get-q-amplitude get-q-pure-states))

(defun get-q-width (qreg)
  (declare (type quantum-register qreg))
  (quantum-register-width qreg))

(defun get-q-l0-norm (qreg)
  (declare (type quantum-register qreg))
  (quantum-register-l0-norm qreg))

(defun get-q-amplitude (qreg)
  (declare (type quantum-register qreg))
  (quantum-register-amplitude qreg))

(defun get-q-pure-states (qreg)
  (declare (type quantum-register qreg))
  (quantum-register-pure-states qreg))

(declaim (inline (setf get-q-width)
		 (setf get-q-l0-norm)
		 (setf get-q-amplitude)
		 (setf get-q-pure-states)))

(defun (setf get-q-width) (width qreg)
  (declare (type uint width)
	   (type quantum-register qreg))
  (setf (quantum-register-width qreg) width))

(defun (setf get-q-l0-norm) (l0-norm qreg)
  (declare (type uint l0-norm)
	   (type quantum-register qreg))
  (setf (quantum-register-l0-norm qreg) l0-norm))

(defun (setf get-q-amplitude) (amplitude qreg)
  (declare (type (vector amplitude) amplitude)
	   (type quantum-register qreg))
  (setf (quantum-register-amplitude qreg) amplitude))

(defun (setf get-q-pure-states) (pstates qreg)
  (declare (type (vector uint) pstates)
	   (type quantum-register qreg))
  (setf (quantum-register-pure-states qreg) pstates))

(declaim (inline make-matrix make-square-matrix))
(defun make-matrix (dimensions &key (element-type t) initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset)
  (make-array dimensions element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset))

(defun make-square-matrix (dimension &key (element-type t) initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset)
  (make-matrix '(,dimension ,dimension) element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset))

(defun unitary-matrix-p (matrix)
  "Predicate of unitary matrix."
  (declare (type square-matrix matrix))
  (if (identity-matrix-p (matrix-* (matrix-adjoint matrix) matrix))
      t
      nil))

(defun identity-matrix-p (matrix)
  "Predicate of identity matrix."
  (declare (type square-matrix matrix))
  (let ((row (array-dimension matrix 0)))
    (loop for i from 0 to (- row 1) do
	 (if (/= (aref matrix i i) 1)
	     (return-from identity-matrix-p nil)))
    t))

(defun trace-zero-p (matrix)
  "Predicate of zero trace matrix."
  (declare (type square-matrix matrix))
  (let ((row (array-dimension matrix 0))
	(sum 0))
    (loop for i from 0 to (- row 1) do
	 (incf sum (aref matrix i i)))
    (if (= 0 sum)
	t
	nil)))

(defun hermitian-p (matrix)
  "Predicate of Hermitian matrix."
  (declare (type square-matrix matrix))
  (if (= (matrix-adjoint matrix) matrix)
      t
      nil))