;;;; The MIT License (MIT)

;;;; Copyright (c) 2015 Huang Xuxing

;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

;;;; Shor algorithm
;;;; Date: June 18, 2015

(in-package :cl-quantum)

(defun shor (number)
  "Shor algorithm wrapper, will automatically restart if failed."
  (block shor-loop
    (loop
       (shor-algorithm number))))

(defun shor-algorithm (number)
  "Shor algorithm. Decomposite 'number' into factors."
  (let* ((w1 (qubits (* number number)))
	 (w2 (qubits number))
	 (width (+ w1 (* 3 w2) 2))
	 (qreg (make-quantum-register :width w1))
	 (rand (random number))
	 (out 0)
	 (denom 0)
	 (plus-one 0)
	 (minus-one 0))
    (when (or (= rand 2)
	      (> (gcd rand number) 1))
      (setf rand (random number)))
    (loop for i from 0 to (1- w1) do
	 (hadamard i qreg))
    ;; After hadamard gates add helper qubits to the register.
    (incf (get-q-width qreg) (+ 2 (* 3 w2)))
    (q-expt-mod number rand w1 w2 qreg)
    (loop for i from 0 to (1- (+ 2 (* 3 w2))) do
	 (measure-qubit 0 qreg))
    (qft w1 qreg)
    (loop for i from 0 to (/ width 2) do
	 (c-not i (- w1 i 1) qreg)
	 (c-not (- w1 i 1) i qreg)
	 (c-not i (- w1 i 1) qreg))
    (setf out (measure qreg))
    (if (= out 0)
	;; Failed, and how about run it again instead of inform the user?
	(progn
	  (format t "Try again.")
	  (go shor-loop)))
    (setf denom (ash 1 width))
    (format t "Output ~D (~F)." out (/ out denom))
    (if (and (= 1 (mod denom 2))
	     (< (* 2 denom) (ash 1 width)))
	(setf denom (* 2 denom)))
    (if (= 1 (mod denom 2))
	(progn
	  (format t "Odd period. Try again.")
	  (go shor-loop)))
    (setf plus-one (gcd number (mod (+ 1 (expt rand (/ 2 q))) number))
	  minus-one (gcd number (mod (- (expt rand (/ 2 q)) 1) number)))
    (let ((candidate (max plus-one minus-one)))
      (if (and (> candidate 1)
	       (< candidate number))
	  (progn
	    (format t "~D = ~D * ~D" number candidate (/ number candidate))
	    (return-from shor (values candidate (/ number candidate))))
	  (progn
	    (format t "Try again.")
	    (go shor-loop))))
    ;; Won't reach here.
    (go shor-loop)))
