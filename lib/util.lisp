;;;; Utilities
;;;; Author: Augustus Huang
;;;; Date: May 22, 2015

(in-package :general-utilities)

(defun map-vec (fn vector &key (start 0) end)
  "Helper function of dovec."
  (loop for i from start below (or end (length vector))
       do (funcall fn (aref vector-var index))))

(defun matrix-*-2 (matrix1 matrix2)
  "Helper of matrix-*."
  (declare (type matrix matrix1)
	   (type matrix matrix2))
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
  (let* ((row (first (array-dimensions matrix1)))
	 (column (second (array-dimensions matrix1)))
	 (matrix3 (make-array `(,row ,column) :initial-element 0)))
    (loop for i from 0 to (- row 1) do
	 (loop for j from 0 to (- column 1) do
	      (setf (aref matrix3 i j)
		    (+ (aref matrix1 i j) (aref matrix2 i j)))))
    matrix3))

(defun matrix---2 (matrix1 matrix2)
  "Helper of matrix--."
  (declare (type matrix matrix1)
	   (type matrix matrix2))
  (let* ((row (first (array-dimensions matrix1)))
	 (column (second (array-dimensions matrix1)))
	 (matrix3 (make-array `(,row ,column) :initial-element 0)))
    (loop for i from 0 to (- row 1) do
	 (loop for j from 0 to (- column 1) do
	      (setf (aref matrix3 i j)
		    (- (aref matrix1 i j) (aref matrix2 i j)))))
    matrix3))

;;; APIs
(defmacro with-gensyms (symbols body)
  "Replace given symbols with gensyms."
  (sublis (mapcar #'(lambda (sym)
		      (cons sym (gensym (string sym))))
		  symbols)
	  body))

(defmacro gethash-or-set-default (key table default)
  "Get the value from table or set it to default value."
  (with-gensyms (keyvar tabvar val found-p)
    `(let ((keyvar ,key)
	   (tabvar ,table))
       (multiple-value-bind (val found-p)
	   (gethash keyvar tabvar)
	 (if found-p
	     val
	     (setf (gethash keyvar tabvar)
		   ,default))))))

(defmacro dovec ((var vector &key (start 0) end) &body body)
  "Vector version of 'doxxx' macros."
  `(block nil
     (map-vec #'(lambda (,var) ,@body)
	      ,vector :start start :end end)))

(defmacro defun-memo (fn-name-options (&rest args) &body body)
  "Remember what have been done with 'this' function."
  (let ((vars (arglist-vars args)))
    (flet ((gen-body (fn-name &key (test '#'equal)
			      size key-exp)
	     `(eval-when (load eval compile)
		(setf (get ',fn-name 'memoize-table)
		      (make-hash-table :test ,test
				       ,@(when size `(:size ,size))))
		(defun ,fn-name ,args
		  (gethash-or-set-default
		   ,key-exp
		   (get ',fn-name 'memoize-table)
		   (progn ,@body))))))
      (cond ((consp fn-name-options)
	     (apply #'gen-body fn-name-options))
	    ((and (= (length vars) 1)
		  (not (member '&rest args)))
	     (gen-body fn-name-options :test '#'eql
		       :key-exp (first vars)))
	    (t
	     (gen-body fn-name-options :test '#'equal
		       :key-exp `(list* ,@vars)))))))

(defun transpose (matrix)
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

(defun conjugate (matrix)
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

(defun adjoint (matrix)
  "Returns the adjoint of a given matrix."
  (declare (type matrix matrix))
  (funcall conjugate (funcall transpose matrix)))

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