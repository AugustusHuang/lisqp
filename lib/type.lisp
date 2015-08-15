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

;;;; Generic useful type information

(in-package :cl-lisqp-util)

(deftype uint ()
  "Unsigned fixnum integer."
  `(integer 0 ,most-positive-fixnum))

(deftype uint-general ()
  '(integer 0 *))

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
  (amplitudes #(#C(1 0)) :type (vector complex))
  (pure-states #(0) :type vector))

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
  (quantum-register-width qreg))

(defun get-q-l0-norm (qreg)
  (quantum-register-l0-norm qreg))

(defun get-q-amplitudes (qreg)
  (quantum-register-amplitudes qreg))

(defun get-q-pure-states (qreg)
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
  (declare (type uint width))
  (setf (quantum-register-width qreg) width))

(defun (setf get-q-l0-norm) (l0-norm qreg)
  (declare (type uint l0-norm))
  (setf (quantum-register-l0-norm qreg) l0-norm))

(defun (setf get-q-amplitudes) (amplitudes qreg)
  (declare (type (vector complex) amplitudes))
  (setf (quantum-register-amplitudes qreg) amplitudes))

(defun (setf get-q-pure-states) (pstates qreg)
  (declare (type (vector uint) pstates))
  (setf (quantum-register-pure-states qreg) pstates))

(defun list-dimensions (list depth)
  "List counterpart of function ARRAY-DIMENSIONS."
  (loop repeat depth
     collect (length list)
     do (setf list (car list))))

(defun list-to-array (list depth)
  "Make an array from a given list."
  (make-array (list-dimensions list depth) :initial-contents list))

(defun 1d-array-to-list (array)
  "Make a list from an 1-dimensional array."
  (loop for i below (array-dimension array 0) collect (aref array i)))

(defun remove-by-position (index list)
  "REMOVE function by index."
  (loop for i in list
     for j from 1 unless (= j (1+ index)) collect i))

(defun insert (list index value)
  "Insert VALUE before the INDEXth element in the list."
  (let ((dis (1- index)))
    (push value (cdr (nthcdr dis list)))
    list))

(defun (setf aref-sparse-vector) (value svec index)
  "(SETF AREF) function of a sparse vector."
  (declare (type sparse-vector svec)
	   (type fixnum index))
  (let ((index-array (sparse-vector-indices svec))
	(values (sparse-vector-values svec))
	(len (sparse-vector-len svec))
	(helper-list1 ())
	(helper-list2 ()))
    (assert (< index len)
	    (value svec index)
	    "Index ~D out of bounds of sparse vector ~A."
	    index
	    svec)
    (loop for i from 0 to (1- (length index-array)) do
	 (let ((ind (aref index-array i)))
	   ;; There are chances that the last index is smaller than the index
	   ;; of where we wanna change the value...
	   (if (<= index ind)
	       ;; If we meet a slot with non-zero value, substitute it or die.
	       (if (= index ind)
		   (if (/= value 0)
		       (progn
			 (setf (aref values index) value)
			 (return-from aref-sparse-vector value))
		       ;; Remove this slot from values and index.
		       (progn
			 (setf helper-list1
			       (remove-by-position ind (1d-array-to-list values))
			       helper-list2
			       (remove-by-position ind (1d-array-to-list index-array))
			       (sparse-vector-values svec)
			       (list-to-array helper-list1 1)
			       (sparse-vector-indices svec)
			       (list-to-array helper-list2 1))
			 (return-from aref-sparse-vector value)))
		   ;; The first time we meet a index greater than our goal.
		   (if (/= value 0)
		       ;; Add a new index and form a new values array.
		       (progn
			 (setf helper-list1
			       (insert (1d-array-to-list values) i value)
			       helper-list2
			       (insert (1d-array-to-list index-array) i index)
			       (sparse-vector-values svec)
			       (list-to-array helper-list1 1)
			       (sparse-vector-indices svec)
			       (list-to-array helper-list2 1))
			 (return-from aref-sparse-vector value))
		       ;; Since this is the first time index >= our place,
		       ;; So there's no chance to change anything, we are done.
		       (return-from aref-sparse-vector value))))))
    ;; The biggest index is smaller than ours, append it and the value.
    (setf helper-list1 (append (1d-array-to-list values) (list value))
	  helper-list2 (append (1d-array-to-list index-array) (list index))
	  (sparse-vector-values svec) (list-to-array helper-list1 1)
	  (sparse-vector-indices svec) (list-to-array helper-list2 1))
    value))

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
		 (push i indices)))))
    (make-sparse-vector :values (list-to-array (reverse values) 1)
			:indices (list-to-array (reverse indices) 1)
			:len len)))
