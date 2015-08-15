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

;;;; Different classical types in quantum register representation

(in-package :cl-quantum-emulator)

(defclass quantum-generic ()
  (:documentation
   "Generic quantum object class, used only as a superclass, don't create concrete objects of this class."))

;;; Since quantum registers are binary, you can regard an integer as a binary
;;; number or anything else. It depends on how to interpret it.
(defclass quantum-integer (quantum-generic)
  ((int :initarg :int
	:type quantum-register
	:accessor int))
  (:documentation
   "Quantum integer class, use a quantum register to represent integer value."))

(defclass quantum-ratio (quantum-generic)
  ((numerator :initarg :numerator
	      :type quantum-register
	      :accessor numerator)
   (denominator :initarg :denominator
		:type quantum-register
		:accessor denominator))
  (:documentation
   "Quantum ratio class, use two quantum registers to represent integer denominator and numerator."))

(defclass quantum-float (quantum-generic)
  ((float :initarg :float
	  :type quantum-register
	  :accessor float)
   ;; Here point means nth count from tail, e.g. 101.01 has point 2.
   (point :initarg :point
	  :type integer
	  :accessor point))
  (:documentation
   "Quantum float class, use a quantum register and a classic integer to represent float magnitude and ratio."))

(defgeneric binary-q-+ (num1 num2)
  (:documentation "Generic binary quantum add function."))

(defgeneric binary-q-*-mod (num1 num2 mod)
  (:documentation "Generic binary quantum multiply-mod function."))

(defgeneric binary-q-+-mod (num1 num2 mod)
  (:documentation "Generic binary quantum add-mod function."))

(defgeneric binary-q-expt-mod (num1 num2 mod)
  (:documentation "Generic binary quantum expt-mod function."))

;;; There are three levels of functions: quantum library level,
;;; quantum emulator level and abstracted class level. Here we are on the 3rd.
(defmethod binary-q-+ ((num1 quantum-integer) (num2 quantum-integer))
  ;; Directly apply QE level add function.
  (make-instance 'quantum-integer :int (%-+ (int num1) (int num2))))

(defmethod binary-q-+ ((num1 quantum-ratio) (num2 quantum-ratio))
  ;; A/B + C/D = (A*D+B*C)/B*D
  (make-instance 'quantum-ratio
		 :numerator (%-+ (%-*-mod (numerator num1)
					  (denominator num2)
					  *bignum-register*)
				 (%-*-mod (denominator num1)
					  (numerator num2)
					  *bignum-register*))
		 :denominator (%-*-mod (denominator num1)
				       (denominator num2)
				       ;; Choose a big modulus.
				       *bignum-register*)))

;;; Functions lack: INTEGER-PART.
(defmethod binary-q-+ ((num1 quantum-float) (num2 quantum-float))
  ;; Add like integer, then move point around.
  (make-instance 'quantum-float
		 :float
		 :point))

(defmethod binary-q-+ ((num1 quantum-integer) (num2 quantum-ratio))
  ;; A/1+B/C = (AC+B)/C
  (make-instance 'quantum-ratio
		 :numerator (%-+ (%-*-mod (int num1)
					  (denominator num2)
					  *bignum-register*)
				 (numerator num2))
		 :denominator (denominator num2)))

(defmethod binary-q-+ ((num1 quantum-integer) (num2 quantum-float))
  )

(defmethod binary-q-+ ((num1 quantum-ratio) (num2 quantum-integer))
  ;; A/B+C/1 = (A+BC)/B
  (make-instance 'quantum-ratio
		 :numerator (%-+ (numerator num1)
				 (%-*-mod (denominator num1)
					  (int num2)
					  *bignum-register*))
		 :denominator (denominator num1)))

(defmethod binary-q-+ ((num1 quantum-ratio) (num2 quantum-float))
  ;; Consider a float as a generalized ratio.
  )

(defmethod binary-q-+ ((num1 quantum-float) (num2 quantum-integer))
  )

(defmethod binary-q-+ ((num1 quantum-float) (num2 quantum-ratio))
  )

;; Modulus can only be of type QUANTUM-INTEGER. So do arguments.
(defmethod binary-q-+-mod ((num1 quantum-integer) (num2 quantum-integer) (mod quantum-integer))
  (make-instance 'quantum-integer :int (%-+-mod (int num1) (int num2)
						*bignum-register*)))

(defmethod binary-q-*-mod ((num1 quantum-integer) (num2 quantum-integer) (mod quantum-integer))
  (make-instance 'quantum-integer :int (%-*-mod (int num1) (int num2)
						*bignum-register*)))

(defmethod binary-q-expt-mod ((num1 quantum-integer) (num2 quantum-integer) (mod quantum-integer))
  (make-instance 'quantum-integer :int (%-expt-mod (int num1) (int num2)
						   *bignum-register*)))

