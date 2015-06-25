;;;; Generic useful type information
;;;; Author: Augustus Huang
;;;; Date: May 22, 2015

(in-package :general-utilities)

(deftype uint ()
  "Unsigned fixnum integer."
  '(integer 0 most-positive-fixnum))

(deftype uint-general ()
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
  (width 1 :type uint)
  (l0-norm 1 :type uint)
  (amplitudes #(1) :type (vector complex))
  (pure-states #(0) :type (vector uint)))

(defstruct sparse-vector
  "Sparse vector in trivial CSR format."
  (values (make-array 0) :type simple-array)
  (indices (make-array 0 :element-type 'uint) :type simple-array)
  (len 0))

(declaim (inline get-q-width
		 get-q-l0-norm
		 get-q-amplitudes
		 get-q-pure-states
		 aref-sparse-vector))

(defun get-q-width (qreg)
  (declare (type quantum-register qreg))
  (quantum-register-width qreg))

(defun get-q-l0-norm (qreg)
  (declare (type quantum-register qreg))
  (quantum-register-l0-norm qreg))

(defun get-q-amplitudes (qreg)
  (declare (type quantum-register qreg))
  (quantum-register-amplitudes qreg))

(defun get-q-pure-states (qreg)
  (declare (type quantum-register qreg))
  (quantum-register-pure-states qreg))

(defun aref-sparse-vector (svec index)
  "Aref function of a sparse vector."
  (declare (type sparse-vector svec)
	   (type fixnum index))
  (loop for i from 0 to (1- (length (sparse-vector-indices svec))) do
       (if (= index (aref (sparse-vector-indices svec) i))
	   (return-from aref-sparse-vector (aref (sparse-vector-values svec) i))))
  0)

(declaim (inline (setf get-q-width)
		 (setf get-q-l0-norm)
		 (setf get-q-amplitudes)
		 (setf get-q-pure-states)
		 (setf aref-sparse-vector)))

(defun (setf get-q-width) (width qreg)
  (declare (type uint width)
	   (type quantum-register qreg))
  (setf (quantum-register-width qreg) width))

(defun (setf get-q-l0-norm) (l0-norm qreg)
  (declare (type uint l0-norm)
	   (type quantum-register qreg))
  (setf (quantum-register-l0-norm qreg) l0-norm))

(defun (setf get-q-amplitudes) (amplitudes qreg)
  (declare (type (vector complex) amplitudes)
	   (type quantum-register qreg))
  (setf (quantum-register-amplitudes qreg) amplitudes))

(defun (setf get-q-pure-states) (pstates qreg)
  (declare (type (vector uint) pstates)
	   (type quantum-register qreg))
  (setf (quantum-register-pure-states qreg) pstates))

(defun (setf aref-sparse-vector) (value svec index)
  "(setf aref) function of a sparse vector."
  (declare (type sparse-vector svec)
	   (type fixnum index))
  (loop for i from 0 to (1- (length (sparse-vector-indices svec))) do
       (let ((ind (aref (sparse-vector-indices svec) i)))
	 (if (<= index ind)
	     ;; Just substitute it...
	     (if (= index ind)
		 (progn
		   (setf (aref (sparse-vector-values svec) index) value)
		   (return-from aref-sparse-vector))
		 ;; The first time we meet a index greater than our goal.
		 (let ((helper1 ())
		       (helper2 ())
		       (values-list (1d-array-to-list (sparse-vector-values svec)))
		       (index-list (1d-array-to-list (sparse-vector-indices svec))))
		   (loop for j from 0 to (- i 2) do
			(push (pop values-list) helper1)
			(push (pop index-list) helper2))

		   (push value helper1)
		   (push index helper2)

		   (loop for j from 0 to (1- i) do
			(push (pop helper1) values-list)
			(push (pop helper2) index-list))
		   (setf (sparse-vector-values svec) (list-to-array values-list 1)
			 (sparse-vector-indices svec) (list-to-array index-list 1))))))))

(defun make-qreg-with-vector (vec &key (start 0) (end (1- (length vec))))
  "Make a quantum register with content of a vector."
  (declare (type vector vec))
  (let* ((width (1+ (- end start)))
	 (qreg (make-quantum-register :width width))
	 (l0 0)
	 (amp-list ())
	 (state-list ()))
    (loop for i from start to end do
	 (let ((item (svref vec i)))
	   (if (/= item 0)
	       (progn
		 (incf l0)
		 (push i state-list)
		 (push item amp-list)))))
    (setf (get-q-pure-states qreg) (make-array (list-dimensions state-list 1)
					       :initial-contents (reverse state-list))
	  (get-q-amplitudes qreg) (make-array (list-dimensions amp-list 1)
					      :initial-contents (reverse amp-list))
	  (get-q-l0-norm qreg) l0)
    qreg))

(defun make-qreg-with-sparse-vector (svec)
  "Make a quantum register with content of a sparse vector."
  (declare (type sparse-vector svec))
  (make-quantum-register :width (sparse-vector-len svec)
			 :l0-norm (length (sparse-vector-indices svec))
			 :amplitudes (sparse-vector-values svec)
			 :pure-states (sparse-vector-indices svec)))

(defun make-sparse-vector-with-vector (vector)
  "Make function of a sparse vector with a generic vector."
  (declare (type vector vector))
  (let ((len (length vector))
	(values ())
	(indices ()))
    (loop for i from 0 to (1- len) do
	 (let ((value (aref vector i)))
	   (if (/= 0 value)
	       (progn
		 (push value values)
		 (push i index)))))
    (make-sparse-vector :values (list-to-array (reverse values) 1)
			:indices (list-to-array (reverse indices) 1)
			:len len)))

;;; Matrix wrappers.
(declaim (inline make-matrix make-square-matrix))
(defun make-matrix (dimensions &key (element-type t) initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset)
  (make-array dimensions element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset))

(defun make-square-matrix (dimension &key (element-type t) initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset)
  (make-matrix '(,dimension ,dimension) element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset))

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
  (if (= (matrix-adjoint matrix) matrix)
      t
      nil))
