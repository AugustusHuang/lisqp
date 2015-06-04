;;;; Decoherence of quantum states
;;;; Author: Augustus Huang
;;;; Date: May 25, 2015

(in-package :cl-quantum)

;;; Decoherence level, 0 means disabled.
(defparameter *decoherence-level* 0)

(declaim (inline get-decoherence-level))
(defun get-decoherence-level ()
  "Get current decoherence level, 0 means decoherence not enabled."
  *decoherence-level*)

(declaim (inline set-decoherence-level))
(defun set-decoherence-level (level)
  "Set current decoherence level to level, above 1 means decoherence enabled."
  (setf *decoherence-level* level))

;;; TODO: If we can make it a blackbox without side-effect?
(defun decohere (qreg)
  "Decohere. This is a destructive method."
  (declare (type quantum-register qreg))
  (let* ((rand (make-array (get-q-width qreg) :element-type double-float))
	 (width (get-q-width qreg))
	 (l0 (get-q-l0-norm qreg))
	 (alpha (- (* 2 (normalised-random most-positive-fixnum) 1)))
	 (beta (- (* 2 (normalised-random most-positive-fixnum) 1)))
	 (amp (+ (* alpha alpha) (* beta beta))))
    (loop for i from 0 to (- width 1) do
	 (when (>= amp 1)
	   (setf alpha (- (* 2 (normalised-random most-positive-fixnum) 1))
		 beta (- (* 2 (normalised-random most-positive-fixnum) 1))
		 amp (+ (* alpha alpha) (* beta beta))))
	 
	 (setf (svref rand i) (* alpha
				 (sqrt (* 2 (get-decoherence-level)))
				 (sqrt (* (/ (log amp) amp) -2)))))
    (loop for i from 0 to (- l0 1) do
	 (let ((angle 0))
	   (loop for j from 0 to (- width 1) do
		(if (logtest (svref (get-q-pure-states qreg) i)
			     (ash 1 j))
		    (incf angle (svref rand j))
		    (decf angle (svref rand j))))
	   (setf (svref (get-q-amplitude qreg) i) (* (svref (get-q-amplitude qreg))
						     (demoivre angle)))))))
