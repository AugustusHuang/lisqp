;;;; Utilities
;;;; Author: Augustus Huang
;;;; Date: May 22, 2015

(in-package :general-utilities)

(defun matrix-*-2 (matrix1 matrix2)
  "Helper of matrix-*."
  (declare (type matrix matrix1)
	   (type matrix matrix2))
  (let ((m1-row (first (array-dimensions matrix1)))
	(m1-column (second (array-dimensions matrix1)))
	(m2-row (first (array-dimensions matrix2)))
	(m2-column (second (array-dimensions matrix2))))
    (if (/= m1-column m2-row)
	(error "size mismatch")
	(let ((matrix3 (make-array `(,m1-row ,m2-column) :initial-element 0)))
	  (loop for i from 0 to (- m1-row 1) do
	       (loop for j from 0 to (- m2-column 1) do
		    (setf (aref matrix3 i j)
			  (loop for k from 0 to (- m1-column 1)
			     sum (* (aref matrix1 i k)
				    (aref matrix2 k j))))))
	  matrix3))))

(defun matrix-+-2 (matrix1 matrix2)
  "Helper of matrix-+."
  (declare (type matrix matrix1)
	   (type matrix matrix2))
  (let ((m1-row (first (array-dimensions matrix1)))
	(m1-column (second (array-dimensions matrix1)))
	(m2-row (first (array-dimensions matrix2)))
	(m2-column (second (array-dimensions matrix2))))
    (if (or (/= m1-row m2-row)
	    (/= m1-column m2-column))
	(error "size mismatch")
	(let ((matrix3 (make-array `(,m1-row ,m1-column) :initial-element 0)))
	  (loop for i from 0 to (- m1-row 1) do
	       (loop for j from 0 to (- m1-column 1) do
		    (setf (aref matrix3 i j)
			  (+ (aref matrix1 i j) (aref matrix2 i j)))))
	  matrix3))))

(defun matrix---2 (matrix1 matrix2)
  "Helper of matrix--."
  (declare (type matrix matrix1)
	   (type matrix matrix2))
  (let ((m1-row (first (array-dimensions matrix1)))
	(m1-column (second (array-dimensions matrix1)))
	(m2-row (first (array-dimensions matrix2)))
	(m2-column (second (array-dimensions matrix2))))
    (if (or (/= m1-row m2-row)
	    (/= m1-column m2-column))
	(error "size mismatch")
	(let ((matrix3 (make-array `(,m1-row ,m1-column) :initial-element 0)))
	  (loop for i from 0 to (- m1-row 1) do
	       (loop for j from 0 to (- m1-column 1) do
		    (setf (aref matrix3 i j)
			  (- (aref matrix1 i j) (aref matrix2 i j)))))
	  matrix3))))

;;; APIs
(defmacro with-gensyms (symbols body)
  "Replace given symbols with gensyms."
  (sublis (mapcar #'(lambda (sym)
		      (cons sym (gensym (string sym))))
		  symbols)
	  body))

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
	 (out (make-array '(,row ,column) :initial-element 0)))
    (loop for i from 0 to (- row 1) do
	 (loop for j from 0 to (- column 1) do
	      (setf (aref out i j)
		    (conjugate (aref matrix i j)))))
    out))

(defun matrix-adjoint (matrix)
  "Returns the adjoint of a given matrix."
  (declare (type matrix matrix))
  (funcall matrix-conjugate (funcall matrix-transpose matrix)))

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
  (let ((dim (array-dimension vector1 0))
	(result 0))
    (if (not (= dim (array-dimension vector2 0)))
	     (error "vectors are of different size"))
    (loop for i from 0 to (- dim 1) do
	 (incf result (* (svref vector1 i) (svref vector2 i))))
    result))

(defun kronecker-product (vector1 vector2)
  "Kronecker product of two general vectors."
  (declare (type vector vector1 vector2))
  (let (())))

(defun vec-*-matrix (vec matrix)
  "Product of a vector and a matrix."
  (declare (type vector vec)
	   (type matrix matrix))
  (let* ((row (array-dimension matrix 0))
	 (column (array-dimension matrix 1))
	 (out (make-array '(,column) :initial-element 0)))
    (if (not (= (array-dimension vec 0) row))
	(error "vector and matrix size mismatch"))
    (loop for i from 0 to (- column 1) do
	 (loop for j from 0 to (- row 1) do
	      (incf (svref out i) (* (svref vec j) (aref matrix j i)))))
    out))

(defun demoivre (angle)
  (complex (cos angle) (sin angle)))

(defun list-dimensions (list depth)
  "Count the dimension of a list."
  (loop repeat depth
       collect (length list)
       do (setf list (car list))))

(defun list-to-array (list depth)
  "Make an array from a given list."
  (make-array (list-dimensions list depth) :initial-contents list))

(defun 1d-array-to-list (array)
  "Make a list from an 1-dimensional array."
  (loop for i below (array-dimension array 0) collect (aref array i)))

(defun inverse-mod (n a)
  "Inverse of a mod n."
  (loop for i from 1 to (1- n) do
       (if (= 1 (mod (* i a) n))
	   (return-from inverse-mod i))))
