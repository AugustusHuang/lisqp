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

;;;; General arithmetic functions for emulator

(in-package :cl-quantum-emulator)

(defun make-1-to-n-vector (n)
  "Helper function, make a vector from 1 to N."
  (let ((out (make-array n :initial-element 0)))
    (loop for i from 0 to (1- n) do
	 (setf (aref out i) (1+ i)))
    out))

;;; This is a 'BIGNUM' in quantum register sense, we use this to achieve
;;; trivial multiplication and expt (we don't expect big input now!)
;;; Pick arguments randomly... 2^32 will be enough?
(defparameter *bignum-register*
  (make-quantum-register :width 64
			 :l0-norm 32
			 :amplitudes (make-array 32 :initial-element #C(1 0))
			 :pure-states (make-1-to-n-vector 32)))

(defun %-+ (qreg1 qreg2)
  "Binary helper function of add algorithm."
  (declare (type quantum-register qreg1 qreg2))
  )

(defun %-+-mod (qreg1 qreg2 mod)
  "Binary helper function of add-mod algorithm."
  (declare (type quantum-register qreg1 qreg2 mod))
  )

(defun %-*-mod (qreg1 qreg2 mod)
  "Binary helper function of mul-mod algorithm."
  (declare (type quantum-register qreg1 qreg2 mod))
  )

(defun %-expt-mod (qreg1 qreg2 mod)
  "Binary helper function of expt-mod algorithm."
  (declare (type quantum-register qreg1 qreg2 mod))
  )

