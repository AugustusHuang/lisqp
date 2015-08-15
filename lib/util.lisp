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

;;;; Utilities

(in-package :cl-lisqp-utilities)

;;; Since our matrix won't be too big?
(defun matrix-*-2 (matrix1 matrix2)
  "Helper of matrix-*."
  (declare (type matrix matrix1)
	   (type matrix matrix2))
  (assert (= (array-dimension matrix1 1)
	     (array-dimension matrix2 0))
	  (matrix1 matrix2)
	  "Size mismatch, two matrices of size ~D-by-~D and ~D-by-~D."
	  (array-dimension matrix1 0)
	  (array-dimension matrix1 1)
	  (array-dimension matrix2 0)
	  (array-dimension matrix2 1))
  (let* ((m1-row (first (array-dimensions matrix1)))
	 (m1-column (second (array-dimensions matrix1)))
	 (m2-column (second (array-dimensions matrix2)))
	 (matrix3 (make-array `(,m1-row ,m2-column) :initial-element 0)))
    (loop for i from 0 to (- m1-row 1) do
	 (loop for j from 0 to (- m2-column 1) do
	      (setf (aref matrix3 i j)
		    (loop for k from 0 to (- m1-column 1)
		       sum (* (aref matrix1 i k)
			      (aref matrix2 k j))))))
    matrix3))

(defun matrix-+-2 (matrix1 matrix2)
  "Helper of matrix-+."
  (declare (type matrix matrix1)
	   (type matrix matrix2))
  (assert (and (= (array-dimension matrix1 0)
		  (array-dimension matrix2 0))
	       (= (array-dimension matrix1 1)
		  (array-dimension matrix2 1)))
	  (matrix1 matrix2)
	  "Size mismatch, two matrices of size ~D-by-~D and ~D-by-~D."
	  (array-dimension matrix1 0)
	  (array-dimension matrix1 1)
	  (array-dimension matrix2 0)
	  (array-dimension matrix2 1))
  (let* ((m1-row (first (array-dimensions matrix1)))
	 (m1-column (second (array-dimensions matrix1)))
	 (matrix3 (make-array `(,m1-row ,m1-column) :initial-element 0)))
    (loop for i from 0 to (- m1-row 1) do
	 (loop for j from 0 to (- m1-column 1) do
	      (setf (aref matrix3 i j)
		    (+ (aref matrix1 i j) (aref matrix2 i j)))))
    matrix3))

(defun matrix---2 (matrix1 matrix2)
  "Helper of matrix--."
  (declare (type matrix matrix1)
	   (type matrix matrix2))
  (assert (and (= (array-dimension matrix1 0)
		  (array-dimension matrix2 0))
	       (= (array-dimension matrix1 1)
		  (array-dimension matrix2 1)))
	  (matrix1 matrix2)
	  "Size mismatch, two matrices of size ~D-by-~D and ~D-by-~D."
	  (array-dimension matrix1 0)
	  (array-dimension matrix1 1)
	  (array-dimension matrix2 0)
	  (array-dimension matrix2 1))
  (let* ((m1-row (first (array-dimensions matrix1)))
	 (m1-column (second (array-dimensions matrix1)))
	 (matrix3 (make-array `(,m1-row ,m1-column) :initial-element 0)))
    (loop for i from 0 to (- m1-row 1) do
	 (loop for j from 0 to (- m1-column 1) do
	      (setf (aref matrix3 i j)
		    (- (aref matrix1 i j) (aref matrix2 i j)))))
    matrix3))

(defmacro dovec ((var vector) &body body)
  "Vector version of 'doxxx' macros."
  `(map nil #'(lambda (,var) ,@body) ,vector))

(defun normalised-random (supremum)
  "Get a random number between 0 to 1 (exclusive)."
  (/ (random supremum) supremum))

(defun matrix-transpose (matrix)
  "Returns the transposition of a given matrix."
  (declare (type matrix matrix))
  (let* ((row (first (array-dimensions matrix)))
	 (column (second (array-dimensions matrix)))
	 (out (make-array `(,row ,column) :initial-element 0)))
    (loop for i from 0 to (- row 1) do
	 (loop for j from 0 to (- column 1) do
	      (setf (aref out j i)
		    (aref matrix i j))))
    out))

(defun matrix-conjugate (matrix)
  "Returns the conjugation of a given matrix."
  (declare (type matrix matrix))
  (let* ((row (first (array-dimensions matrix)))
	 (column (second (array-dimensions matrix)))
	 (out (make-array `(,row ,column) :initial-element 0)))
    (loop for i from 0 to (- row 1) do
	 (loop for j from 0 to (- column 1) do
	      (setf (aref out i j)
		    (conjugate (aref matrix i j)))))
    out))

(defun matrix-adjoint (matrix)
  "Returns the adjoint of a given matrix."
  (declare (type matrix matrix))
  (funcall #'matrix-conjugate (funcall #'matrix-transpose matrix)))

(defun matrix-* (matrix1 &rest more-matrices)
  "Returns product of matrices, from left to right."
  (declare (type matrix matrix1))
  (reduce #'matrix-*-2 (cons matrix1 more-matrices)))

(defun matrix-+ (matrix1 &rest more-matrices)
  "Returns sum of matrices."
  (declare (type matrix matrix1))
  (reduce #'matrix-+-2 (cons matrix1 more-matrices)))

(defun matrix-- (matrix1 &rest more-matrices)
  "Returns m1 - (m2 + .. + mn)."
  (declare (type matrix matrix1))
  (reduce #'matrix---2 (cons matrix1 more-matrices)))

(defun qubits (num)
  "Get number of qubits (and amount of superfluous) to represent 'num'."
  (ceiling (log num 2)))

(defun matrix-invert (matrix)
  "Returns the inverse matrix of a given matrix."
  (declare (type square-matrix matrix))
  (let* ((dim (array-dimension matrix 0))
	 (l (make-array dim :initial-element 0))
	 (m (make-array dim :initial-element 0))
	 (temp 0)
	 (det 1)
	 (out (make-array `(,dim ,dim) :initial-element 0)))
    (when (not (equal matrix out))
      (loop for i from 0 to (- dim 1) do
	   (loop for j from 0 to (- dim 1) do
		(setf (aref out i j) (aref matrix i j)))))
    (do ((k 0 (1+ k))
	 (maximum 0)
	 (1/max 0))
	((>= k dim))
      (setf (svref l k) k
	    (svref m k) k
	    maximum (aref out k k))
      (loop for i from k to (- dim 1) do
	   (loop for j from k to (- dim 1) do
		(when (> (abs (aref out i j)) (abs maximum))
		  (setf maximum (aref out i j)
			(svref l k) i
			(svref m k) j))))

      ;; Interchange rows with pivot.
      (if (> (svref l k) k)
	  (do ((j 0 (1+ j))
	       (i (svref l k)))
	      ((>= j dim))
	    (setf temp (- (aref out k j))
		  (aref out k j) (aref out i j)
		  (aref out i j) temp)))
      (if (> (svref m k) k)
	  (do ((i 0 (1+ i))
	       (j (svref m k)))
	      ((>= i dim))
	    (setf temp (- (aref out i k))
		  (aref out i k) (aref out i j)
		  (aref out i j) temp)))
      (if (equalp maximum 0)
	  (return-from matrix-invert 0))
      (setf 1/max (/ 1 maximum))
      (loop for i from 0 to (- dim 1) do
	   (if (not (= i k))
	       (setf (aref out i k)
		     (* (aref out i k) (- 1/max)))))

      ;; Then reduce it.
      (loop for i from 0 to (- dim 1) do
	   (when (not (= i k))
	     (setf temp (aref out i k))
	     (loop for j from 0 to (- dim 1) do
		  (if (not (= j k))
		      (incf (aref out i j)
			    (* temp (aref out k j)))))))

      ;; Divide by pivot row.
      (loop for j from 0 to (- dim 1) do
	   (if (not (= j k))
	       (setf (aref out k j)
		     (* (aref out k j) 1/max))))
      (setf det (* det maximum)
	    (aref out k k) 1/max))

    ;; And finally...
    (loop for k from (1- dim) downto 0 do
	 (if (> (svref l k) k)
	     (do ((j 0 (1+ j))
		  (i (svref l k)))
		 ((>= j dim))
	       (setf temp (aref out j k)
		     (aref out j k) (- (aref out j i))
		     (aref out j i) temp)))
	 (if (> (svref m k) k)
	     (do ((i 0 (1+ i))
		  (j (svref m k)))
		 ((>= i dim))
	       (setf temp (aref out k i)
		     (aref out k i) (- (aref out j i))
		     (aref out j i) temp))))
    (values out det)))

(defun matrix-determinant (matrix)
  "Determinant of a given matrix."
  (declare (type matrix matrix))
  (second (multiple-value-list (matrix-invert matrix))))

(defun inner-product (vector1 vector2)
  "Inner-product of two vectors."
  (declare (type vector vector1)
	   (type vector vector2))
  (assert (= (array-dimension vector1 0)
	     (array-dimension vector2 0))
	  (vector1 vector2)
	  "Size mismatch, two vectors of length ~D and ~D."
	  (array-dimension vector1 0)
	  (array-dimension vector2 0))
  (let ((dim (length vector1))
	(result 0))
    (loop for i from 0 to (- dim 1) do
	 (incf result (* (svref vector1 i) (svref vector2 i))))
    result))

(defun kronecker-product (vector1 vector2)
  "Kronecker product of two general vectors."
  (declare (type vector vector1 vector2))
  (let ((len1 (length vector1))
	(len2 (length vector2)))
    (let* ((len (* len1 len2))
	   (out (make-array len :initial-element 0)))
      (loop for i from 0 to (1- len1) do
	   (loop for j from 0 to (1- len2) do
		(setf (aref out (+ j (* i len2)))
		      (* (aref vector1 i) (aref vector2 j)))))
      out)))

(defun vec-*-matrix (vec matrix)
  "Product of a vector and a matrix."
  (declare (type vector vec)
	   (type matrix matrix))
  (assert (= (array-dimension matrix 0)
	     (array-dimension vec 0))
	  (vec matrix)
	  "Size mismatch, vector of length ~D and matrix of size ~D-by-~D."
	  (array-dimension vec 0)
	  (array-dimension matrix 0)
	  (array-dimension matrix 1))
  (let* ((row (array-dimension matrix 0))
	 (column (array-dimension matrix 1))
	 (out (make-array column :initial-element 0)))
    (loop for i from 0 to (- column 1) do
	 (loop for j from 0 to (- row 1) do
	      (incf (svref out i) (* (svref vec j) (aref matrix j i)))))
    out))

(defun demoivre (angle)
  (complex (cos angle) (sin angle)))

(defun complex-norm (complex)
  (declare (type complex complex))
  (let ((real (realpart complex))
	(imag (imagpart complex)))
    (+ (* real real) (* imag imag))))

(defun inverse-mod (n a)
  "Inverse of a mod n."
  (loop for i from 1 to (1- n) do
       (if (= 1 (mod (* i a) n))
	   (return-from inverse-mod i))))

;;; Matrix predicates.
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
  (flet ((matrix-= (m1 m2)
	   (let ((row (array-dimension m1 0))
		 (col (array-dimension m1 1)))
	     (loop for i from 0 to (1- row) do
		  (loop for j from 0 to (1- col) do
		       (if (/= (aref m1 i j) (aref m2 i j))
			   (return-from matrix-= nil))))
	     t)))
    (matrix-= (matrix-adjoint matrix) matrix)))
