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

(defmethod binary-q-+ ((num1 quantum-integer) (num2 quantum-integer))
  )

(defmethod binary-q-+ ((num1 quantum-ratio) (num2 quantum-ratio))
  )

(defmethod binary-q-+ ((num1 quantum-float) (num2 quantum-float))
  )

(defmethod binary-q-+ ((num1 quantum-integer) (num2 quantum-ratio))
  )

(defmethod binary-q-+ ((num1 quantum-integer) (num2 quantum-float))
  )

(defmethod binary-q-+ ((num1 quantum-ratio) (num2 quantum-integer))
  )

(defmethod binary-q-+ ((num1 quantum-ratio) (num2 quantum-float))
  )

(defmethod binary-q-+ ((num1 quantum-float) (num2 quantum-integer))
  )

(defmethod binary-q-+ ((num1 quantum-float) (num2 quantum-ratio))
  )

;; Modulus can only be of type QUANTUM-INTEGER. So do arguments.
(defmethod binary-q-+-mod ((num1 quantum-integer) (num2 quantum-integer) (mod quantum-integer))
  )

(defmethod binary-q-*-mod ((num1 quantum-integer) (num2 quantum-integer) (mod quantum-integer))
  )

(defmethod binary-q-expt-mod ((num1 quantum-integer) (num2 quantum-integer) (mod quantum-integer))
  )
