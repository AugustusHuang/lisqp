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

;;;; Decoherence of quantum states

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

(defun decohere (qreg)
  "Decoherence routine of a quantum register."
  (declare (type quantum-register qreg))
  (if (not (get-decoherence-level))
      (return-from decohere qreg))
  (let* ((rand (make-array (get-q-width qreg) :element-type 'double-float))
	 (width (get-q-width qreg))
	 (states (get-q-pure-states qreg))
	 (l0 (get-q-l0-norm qreg))
	 (alpha (- (* 2 (normalised-random most-positive-fixnum) 1)))
	 (beta (- (* 2 (normalised-random most-positive-fixnum) 1)))
	 (amp (+ (* alpha alpha) (* beta beta)))
	 (old-amp (get-q-amplitudes qreg))
	 (new-amp (make-array (length old-amp) :initial-contents old-amp)))
    (loop for i from 0 to (1- width) do
	 (if (>= amp 1)
	     (do ()
		 ((< amp 1))
	       (setf alpha (- (* 2 (normalised-random most-positive-fixnum)) 1)
		     beta (- (* 2 (normalised-random most-positive-fixnum)) 1)
		     amp (+ (* alpha alpha) (* beta beta)))))
	 (setf (aref rand i) (coerce (* alpha
					(sqrt (* 2 (get-decoherence-level)))
					(sqrt (* (/ (log amp) amp) -2))) 'double-float)))
    (loop for i from 0 to (1- l0) do
	 (let ((angle 0))
	   (loop for j from 0 to (1- width) do
		(if (logtest (aref states i)
			     (ash 1 j))
		    (incf angle (aref rand j))
		    (decf angle (aref rand j))))
	   (setf (svref new-amp i) (* (svref old-amp i)
				      (demoivre angle)))))
    (make-quantum-register :width width
			   :l0-norm l0
			   :amplitudes new-amp
			   :pure-states (get-q-pure-states qreg))))
