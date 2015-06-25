;;;; Shor algorithm
;;;; Author: Augustus Huang
;;;; Date: June 18, 2015

(in-package :cl-quantum)

(defun shor (number)
  "Shor algorithm. Decomposite 'number' into factors."
  (let* ((w1 (qubits (* number number)))
	 (w2 (qubits number))
	 (width (+ w1 (* 3 w2) 2))
	 (qreg (make-quantum-register :width w1))
	 (rand (random number))
	 (out 0))
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
	  (return-from shor 0))
	(progn
	  ))
    ))
