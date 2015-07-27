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

;;;; Quantum gates
;;;; Date: May 25, 2015

(in-package :cl-quantum)

(defun apply-2-gate (operator qreg target)
  "Generic 2 qubits gate function."
  (declare (type square-matrix operator)
	   (type fixnum target)
	   (type quantum-register qreg))
  (let* ((l0 (get-q-l0-norm qreg))
	 (width (get-q-width qreg))
	 (states (get-q-pure-states qreg))
	 (amps (get-q-amplitudes qreg))
	 (sig (make-array l0 :element-type 'bit :initial-element 0)))
    (block oloop
      (loop for i from 0 to (1- l0) do
	   (let* ((state (aref states i))
		  (amp (aref amps i))
		  (setp 0)
		  (inv 0)
		  (temp amp))
	     (if (not (aref sig i))
		 (let ((offset (boole boole-xor state (ash 1 target))))
		   (setf setp (boole boole-and state (ash 1 target)))
		   (if (>= offset 0)
		       (setf inv offset))
		   (if setp
		       (setf amp (+ (* inv (aref operator 1 0))
				    (* amp (aref operator 1 1))))
		       (setf amp (+ (* inv (aref operator 0 1))
				    (* amp (aref operator 0 0)))))
		   (if (>= offset 0)
		       (if setp
			   (setf (aref amps offset) (+ (* inv (aref operator 0 0))
						       (* amp (aref operator 0 1))))
			   (setf (aref amps offset) (+ (* inv (aref operator 1 1))
						       (* amp (aref operator 1 0)))))
		       ;; Or we have to add a new pure state.
		       (progn
			 (if (or (and (= (aref operator 0 1) 0) setp)
				 (and (= (aref operator 1 0) 0) setp))
			     (return-from oloop nil))
			 (let ((amps-new (adjust-array amps (1+ (length amps))))
			       (states-new (adjust-array states (1+ (length states))))
			   (setf (aref states-new l0)
				 (boole boole-xor (ash 1 target) state))
			   (if setp
			       (setf (aref amps-new l0)
				     (* temp (aref operator 0 1)))
			       (setf (aref amps-new l0)
				     (* temp (aref operator 1 0))))
			   (setf amps amps-new
				 states states-new)))))
		 (if (>= setp 0)
		     (setf (aref sig setp) 1))))))
    (decohere (make-quantum-register :width (max width l0)
				     :l0-norm l0
				     :amplitudes amps
				     :pure-states states)))))

;;; Maybe we need a general apply gate function...
;;; 2n x 2n operator on n qubits.
(defun apply-gate (operator qreg &rest targets)
  "General gate-apply function."
  (declare (type square-matrix operator)
	   (type quantum-register qreg))
  )

(defun c-not (control target qreg)
  "Controlled not gate."
  (declare (type fixnum control target)
	   (type quantum-register qreg))
  (let ((current-width (get-ec-width)))
    (if current-width
	(c-not-ec control target qreg)
	(progn
	  (loop for i from 0 to (1- (get-q-l0-norm qreg)) do
	       (let ((state (aref (get-q-pure-states qreg) i)))
		 (if (logtest state (ash 1 control))
		     (setf state (boole boole-xor state (ash 1 target))))))
	  (decohere qreg)))))

(defun c-not-ec (control target qreg)
  "Controlled not gate with error correction."
  (declare (type fixnum control target)
	   (type quantum-register qreg))
  (let ((current-level (get-decoherence-level))
	(current-width (get-ec-width)))
    (set-decoherence-level 0)
    (set-ec-width 0)
    (c-not control target qreg)
    (c-not (+ control current-width) (+ target current-width) qreg)
    (set-decoherence-level current-level)
    (c-not (+ control (* 2 current-width)) (+ target (* 2 current-width)) qreg)
    (set-ec-width current-width)))

(defun hadamard (target qreg)
  "Hadamard gate."
  (declare (type fixnum target)
	   (type quantum-register qreg))
  (let* ((hadamard-list (list (list (sqrt 1/2) (sqrt 1/2)) (list (sqrt 1/2) (sqrt (- 1/2)))))
	 (operator (make-array '(2 2) :initial-contents hadamard-list)))
    (apply-2-gate operator qreg target)))

(defun toffoli (control1 control2 target qreg)
  "Toffoli gate."
  (declare (type fixnum control1 control2 target)
	   (type quantum-register qreg))
  (let ((current-width (get-ec-width)))
    (if current-width
	(toffoli-ec control1 control2 target qreg)
	(progn
	  (loop for i from 0 to (1- (get-q-l0-norm qreg)) do
	       (let ((state (aref (get-q-pure-states qreg) i)))
		 (if (and (logtest state (ash 1 control1))
			  (logtest state (ash 1 control2)))
		     (setf state (boole boole-xor state (ash 1 target))))))
	  (decohere qreg)))))

(defun toffoli-ec (control1 control2 target qreg)
  "Toffoli gate with error correction."
  (declare (type fixnum control1 control2 target)
	   (type quantum-register qreg))
  (let* ((current-level (get-decoherence-level))
	 (current-width (get-ec-width))
	 (mask (+ (ash 1 target)
		  (ash 1 (+ target current-width))
		  (ash 1 (+ target (* 2 current-width)))))
	 (l0 (get-q-l0-norm qreg))
	 (states (get-q-pure-states qreg)))
    (loop for i from 0 to (- l0 1) do
	 (let ((c1 0)
	       (c2 0))
	   (if (logtest (svref states i) (ash 1 control1))
	       (setf c1 1))
	   (if (logtest (svref states i) (ash 1 (+ control1 current-width)))
	       (setf c1 (boole boole-xor c1 1)))
	   (if (logtest (svref states i) (ash 1 (+ control1 (* 2 current-width))))
	       (setf c1 (boole boole-xor c1 1)))
	   (if (logtest (svref states i) (ash 1 control2))
	       (setf c2 1))
	   (if (logtest (svref states i) (ash 1 (+ control2 current-width)))
	       (setf c2 (boole boole-xor c2 1)))
	   (if (logtest (svref states i) (ash 1 (+ control2 (* 2 current-width))))
	       (setf c2 (boole boole-xor c2 1)))
	   (if (= c1 c2 1)
	       (setf (svref states i) (boole boole-xor (svref states i) mask)))))
    (decohere qreg)))

;;; Swap gate equals three consecutive c-not gates.
(defun swap (control target qreg)
  "Swapping gate."
  (declare (type fixnum control target)
	   (type quantum-register qreg))
  (progn
    (c-not control target qreg)
    (c-not target control qreg)
    (c-not control target qreg)))

(defun phase (target qreg)
  "Phase gate."
  (declare (type fixnum target)
	   (type quantum-register qreg))
  (let ((amplitudes (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- (get-q-l0-norm qreg)) do
	 (let ((state (aref (get-q-pure-states qreg) i)))
	   (if (logtest state (ash 1 target))
	       (setf (aref amplitudes i) (* (aref amplitudes i) #C(0 1))))))
    (decohere qreg)))

(defun pauli-x (target qreg)
  "Pauli x gate."
  (declare (type fixnum target)
	   (type quantum-register qreg))
  (let ((current-width (get-ec-width)))
    (if current-width
	(pauli-x-ec target qreg)
	(progn
	  (loop for i from 0 to (1- (get-q-l0-norm qreg)) do
	       (let ((state (aref (get-q-pure-states qreg) i)))
		 ;; Flip.
		 (setf state (boole boole-xor state (ash 1 target)))))
	  (decohere qreg)))))

(defun pauli-x-ec (target qreg)
  "Pauli x gate with error correction."
  (declare (type fixnum target)
	   (type quantum-register qreg))
  (let ((current-level (get-decoherence-level))
	(current-width (get-ec-width)))
    (set-decoherence-level 0)
    (set-ec-width 0)
    (pauli-x target qreg)
    (pauli-x (+ target current-width) qreg)
    (set-decoherence-level current-level)
    (pauli-x (+ target (* 2 current-width)) qreg)
    (set-ec-width current-width)))

(defun pauli-y (target qreg)
  "Pauli y gate."
  (declare (type fixnum target)
	   (type quantum-register qreg))
  (let ((amplitudes (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- (get-q-l0-norm qreg)) do
	 (let ((state (aref (get-q-pure-states qreg) i)))
	   (setf state (boole boole-xor state (ash 1 target)))
	   (if (logtest state (ash 1 target))
	       ;; Flip and change phase.
	       (setf (aref amplitudes i) (* (aref amplitudes i) #C(0 1)))
	       (setf (aref amplitudes i) (* (aref amplitudes i) #C(0 -1))))))
    (decohere qreg)))

(defun pauli-z (target qreg)
  "Pauli z gate."
  (declare (type fixnum target)
	   (type quantum-register qreg))
  (let ((amplitudes (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- (get-q-l0-norm qreg)) do
	 (let ((state (aref (get-q-pure-states qreg) i)))
	   (if (logtest state (ash 1 target))
	       (setf (aref amplitudes i) (- (aref amplitudes i))))))
    (decohere qreg)))

(defun pi/8 (target qreg)
  "Pi/8 gate."
  (declare (type fixnum target)
	   (type quantum-register qreg))
  (let ((amplitudes (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- (get-q-l0-norm qreg)) do
	 (let ((state (aref (get-q-pure-states qreg) i)))
	   (if (logtest state (ash 1 target))
	       (setf (aref amplitudes i) (* (aref amplitudes i)
					    (demoivre (cos (/ pi 4)) (sin (/ pi 4))))))))
    (decohere qreg)))

(defun c-pauli-z (control target qreg)
  "Controlled pauli z gate."
  (declare (type fixnum control target)
	   (type quantum-register qreg))
  (let ((amplitudes (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- (get-q-l0-norm qreg)) do
	 (let ((state (aref (get-q-pure-states qreg) i)))
	   (if (logtest state (ash 1 control))
	       (if (logtest state (ash 1 target))
		   (setf (aref amplitudes i) (- (aref amplitudes i)))))))
    (decohere qreg)))

(defun c-phase (control target qreg)
  "Controlled phase gate."
  (declare (type fixnum control target)
	   (type quantum-register qreg))
  (let ((amplitudes (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- (get-q-l0-norm qreg)) do
	 (let ((state (aref (get-q-pure-states qreg) i)))
	   (if (logtest state (ash 1 control))
	       (if (logtest state (ash 1 target))
		   (setf (aref amplitudes i) (* (aref amplitudes i) #C(0 1)))))))
    (decohere qreg)))

(defun inverse-c-phase (control target qreg)
  "Inverse controlled phase gate."
  (declare (type fixnum control target)
	   (type quantum-register qreg))
  (let ((amplitudes (get-q-amplitudes qreg)))
    (loop for i from 0 to (1- (get-q-l0-norm qreg)) do
	 (let ((state (aref (get-q-pure-states qreg) i)))
	   (if (logtest state (ash 1 control))
	       (if (logtest state (ash 1 target))
		   (setf (aref amplitudes i) (* (aref amplitudes i) #C(0 -1)))))))
    (decohere qreg)))

;;; Fredkin is controlled-swap.
(defun fredkin (control1 control2 target qreg)
  "Fredkin gate."
  (declare (type fixnum control1 control2 target)
	   (type quantum-register qreg))
  (progn
    (toffoli control1 control2 target qreg)
    (toffoli control1 target control2 qreg)
    (toffoli control1 control2 target qreg)))

;;; Measure will output classical values.
(defun measure (qreg)
  "Measure all qubits in the quantum-register."
  (declare (type quantum-register qreg))
  (let ((rand (normalised-random most-positive-fixnum))
	(states (get-q-pure-states qreg))
	(amps (get-q-amplitudes qreg))
	(l0 (get-q-l0-norm qreg)))
    (loop for i from 0 to (1- l0) do
	 (decf rand (complex-norm (aref amps i)))
	 (if (<= r 0)
	     (return-from measure (aref states i))))
    ;; There's something wrong... Since we have a quantum register with
    ;; total probability less than one, due to lack of normalization or
    ;; some rounding issue.
    ;; TODO: signal a measure error instead.
    (error "measurement failure")))

(defun collapse (value target qreg)
  "Collapse the state vector after measurement or partial trace operator."
  (declare (type fixnum value target)
	   (type quantum-register qreg))
  (let* ((pos (ash 1 target))
	 (norm 0)
	 (l0 (get-q-l0-norm qreg))
	 (states (get-q-pure-states qreg))
	 (out (make-quantum-register :width (1- (get-q-width qreg)))))
    (loop for i from 0 to (1- l0) do
	 (let ((state (aref states i))
	       (amp (aref (get-q-amplitudes qreg) i)))
	   (if (or (and (logtest state pos) value)
		   (and (not (logtest state pos) (not value))))
	       (progn
		 (incf norm (complex-norm amp))
		 (incf l0)))))
    (setf (get-q-l0-norm out) l0
	  (get-q-amplitudes out) (make-array l0 :element-type 'complex
					     :initial-element 0)
	  (get-q-pure-states out) (make-array l0 :element-type 'uint
					      :initial-element 0))
    (loop for i from 0 to (1- l0) do
	 (let ((state (aref states i))
	       (amp (aref (get-q-amplitudes qreg) i)))
	       (counter 0)
	       (lcounter 0)
	       (rcounter 0))
	   (if (or (and (logtest state pos) value)
		   (and (not (logtest state pos) (not value))))
	       (progn
		 (loop for k from 0 to (1- pos) do
		      (incf rcounter (ash 1 k)))
		 (setf rcounter (boole boole-and rcounter state))
		 ;; TODO: Now we are in 32-bit content... make it compatible.
		 (loop for k from 31 downto (1+ pos) do
		      (incf lcounter (ash 1 k)))
		 (setf lcounter (boole boole-and lcounter state)
		       (aref (get-q-amplitudes out) counter) (/ amp (sqrt norm))
		       (aref (get-q-pure-states out) counter)
		       (boole boole-ior rcounter (ash lcounter -1)))
		 (incf counter))))
    out))

(defun measure-qubit (target qreg)
  "Measure a qubit in the quantum-register."
  (declare (type fixnum target)
	   (type quantum-register qreg))
  (let ((pos (ash 1 target))
	(prob 0)
	(out 0)
	(rand (normalised-random most-positive-fixnum))
	(l0 (get-q-l0-norm qreg))
	(states (get-q-pure-states qreg)))
    (loop for i from 0 to (1- l0) do
	 (let ((state (aref states i))
	       (amp (aref (get-q-amplitudes qreg) i)))
	   (if (not (logtest state pos))
	       (incf prob (complex-norm amp)))))
    (if (> rand prob)
	(setf out 1))
    ;; Or we reduce the state vector, and the state will collapse.
    (setf qreg (collapse out target qreg))
    out))
