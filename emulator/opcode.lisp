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

;;;; Opcode manipulation and functions.

(in-package :cl-quantum-emulator)

;;; Version information...
(defconstant +major-version+ 0)
(defconstant +minor-version+ 1)

;;; Do we need to push opcodes to the opcode stack? If T, we are able to
;;; get them back sometime.
(defvar *push-opcodes* nil)

;;; There're two kinds of opcodes: opcodes corresponding to the quantum gates,
;;; and opcodes corresponding to the compound quantum arithmetics.
(defparameter *opcodes*
  '(:opcode-nop
    :opcode-c-not
    :opcode-c-not-ec
    :opcode-hadamard
    :opcode-toffoli
    :opcode-toffoli-ec
    :opcode-swap
    :opcode-phase
    :opcode-pauli-x
    :opcode-pauli-x-ec
    :opcode-pauli-y
    :opcode-pauli-z
    :opcode-pi/8
    :opcode-c-pauli-z
    :opcode-c-phase
    :opcode-inverse-c-phase
    :opcode-fredkin
    :opcode-measure
    :opcode-add
    :opcode-add-mod
    :opcode-times-mod
    :opcode-expt-mod))

(defparameter *opcode-args*
  '(0
    3
    3
    2
    4
    4
    3
    2
    2
    2
    2
    2
    2
    3
    3
    3
    4
    2
    2
    3
    3
    3))

(defun quantum-opcodes (opcode &rest arguments)
  "Quantum opcodes functions."
  (cond ((equal opcode :opcode-nop))
	((equal opcode :opcode-c-not)
	 (c-not (first arguments) (second arguments) (third arguments)))
	((equal opcode :opcode-c-not-ec)
	 (c-not-ec (first arguments) (second arguments) (third arguments)))
	((equal opcode :opcode-hadamard)
	 (hadamard (first arguments) (second arguments)))
	((equal opcode :opcode-toffoli)
	 (toffoli (first arguments) (second arguments) (third arguments) (fourth arguments)))
	((equal opcode :opcode-toffoli-ec)
	 (toffoli-ec (first arguments) (second arguments) (third arguments) (fourth arguments)))
	((equal opcode :opcode-swap)
	 (swap (first arguments) (second arguments) (third arguments)))
	((equal opcode :opcode-phase)
	 (phase (first arguments) (second arguments)))
	((equal opcode :opcode-pauli-x)
	 (pauli-x (first arguments) (second arguments)))
	((equal opcode :opcode-pauli-x-ec)
	 (pauli-x-ec (first arguments) (second arguments)))
	((equal opcode :opcode-pauli-y)
	 (pauli-y (first arguments) (second arguments)))
	((equal opcode :opcode-pauli-z)
	 (pauli-z (first arguments) (second arguments)))
	((equal opcode :opcode-pi/8)
	 (pi/8 (first arguments) (second arguments)))
	((equal opcode :opcode-c-pauli-z)
	 (c-pauli-z (first arguments) (second arguments) (third arguments)))
	((equal opcode :opcode-c-phase)
	 (c-phase (first arguments) (second arguments) (third arguments)))
	((equal opcode :opcode-inverse-c-phase)
	 (inverse-c-phase (first arguments) (second arguments) (third arguments)))
	((equal opcode :opcode-fredkin)
	 (fredkin (first arguments) (second arguments) (third arguments) (fourth arguments)))
	((equal opcode :opcode-measure)
	 (measure (first arguments) (second arguments)))
	((equal opcode :opcode-add)
	 (q-+ (first arguments) (second arguments)))
	((equal opcode :opcode-add-mod)
	 (q-+-mod (first arguments) (second arguments) (third arguments)))
	((equal opcode :opcode-times-mod)
	 (q-*-mod (first arguments) (second arguments) (third arguments)))
	((equal opcode :opcode-expt-mod)
	 (q-expt-mod (first arguments) (second arguments) (third arguments)))
	(t
	 ;; opcode-error condition.
	 (error 'opcode-error :opcode-name opcode :opcode-args arguments))))
	 
;;; Opcodes will be stored in an array. It will be an array of type
;;; (SIMPLE-VECTOR x).
;;; Example: #((OPCODE-1 ARG1 ARG2) '(OPCODE-2) '(OPCODE-3 ARG1))
;;; Now the stack counter is 3 and the frame counter is 2.
(defvar *opcode-vector* (make-array 1 :adjustable t :fill-pointer 0))

;;; Point to the current opcode.
(defvar *opcode-pointer* 0)

;;; *current-q-registers* will contain a list of quantum registers
;;; every quantum register is self-contained and individual.
(defvar *current-q-registers* (vector))

(declaim (inline opcode-push))
(defun opcode-push (opcode)
  (vector-push-extend opcode *opcode-vector*))

(declaim (inline opcode-pop))
(defun opcode-pop (opcode)
  (vector-pop *opcode-vector*))

(declaim (inline opcode-pointer-forward))
(defun opcode-pointer-forward (step)
  "Update opcode stack pointer forward."
  (declare (type fixnum step))
  (incf *opcode-pointer* step))

(declaim (inline opcode-pointer-backward))
(defun opcode-pointer-backward (step)
  "Update opcode stack pointer backward."
  (declare (type fixnum step))
  (if (<= step *opcode-pointer*)
      (decf *opcode-pointer* step)
      (error 'opcode-pointer-error :step step :current *opcode-pointer*)))

(declaim (inline add-q-register))
(defun add-q-register (qreg)
  "Add a new quantum register to current register list."
  (declare (type quantum-register qreg))
  (setf *current-q-registers* (append *current-q-registers* (list qreg))))

(declaim (inline remove-q-register))
(defun remove-q-register (n)
  "Remove a quantum register from the current register list."
  (declare (type fixnum n))
  (setf *current-q-registers* (remove (nth n *current-q-registers*)
				      *current-q-registers*)))

(defun get-nth-q-register (n)
  "Get the nth quantum register in the current register list."
  (declare (type fixnum n))
  (print-quantum-register (nth n *current-q-registers*)))

(declaim (inline set-push-opcodes))
(defun set-push-opcodes (state)
  "Set function of *push-opcodes*."
  (declare (type boolean state))
  (setf *push-opcodes* state))

(declaim (inline get-push-opcodes))
(defun get-push-opcodes ()
  "Get function of *push-opcodes*."
  *push-opcodes*)

;;; FIXME: How about adding frames? If we can add frames then we can tell the
;;; difference among different quantum gate graphs.
(defun input-opcodes ()
  )

(defun output-opcodes ()
  )
