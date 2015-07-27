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

;;;; Opcode manipulation and functions.
;;;; Date: May 25, 2015

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
	 
;;; Opcodes will be stored in a list.
;;; Example: '(((opcode1 arg1 arg2) (opcode2 arg1)) ((opcode3)))
;;; Now the stack counter is 3 and the frame counter is 2.
(defvar *opcode-stack-counter* 0)
(defvar *opcode-frame-counter* 0)

;;; *current-q-registers* will contain a list of quantum registers
;;; every quantum register is self-contained.
(defvar *current-q-registers* (vector))

;;; *opcode-list* will be the list of current running program opcodes.
(defvar *opcode-list* nil)

(declare (inline opcode-stack-forward))
(defun opcode-stack-forward (step)
  "Update opcode stack pointer forward."
  (declare (type fixnum step))
  (incf *opcode-stack-counter* step))

(declare (inline opcode-stack-backward))
(defun opcode-stack-backward (step)
  "Update opcode stack pointer backward."
  (declare (type fixnum step))
  (decf *opcode-stack-counter* step))

(declare (inline opcode-frame-forward))
(defun opcode-frame-forward (step)
  "Update opcode frame pointer forward."
  (declare (type fixnum step))
  (incf *opcode-frame-counter* step))

(declare (inline opcode-frame-backward))
(defun opcode-frame-backward (step)
  "Update opcode frame pointer backward."
  (declare (type fixnum step))
  (decf *opcode-frame-counter* step))

(declare (inline get-nth-opcode-stack))
(defun get-nth-opcode-stack (n)
  "Get the nth opcode in the current opcode list."
  (declare (type fixnum n))
  (caar (nth n *opcode-list*)))

(declare (inline get-nth-opcode-frame))
(defun get-nth-opcode-frame (n)
  "Get the nth opcode frame in the current opcode list."
  (declare (type fixnum n))
  (nth n *opcode-list*))

(declare (inline add-q-register))
(defun add-q-register (qreg)
  "Add a new quantum register to current register list."
  (declare (type quantum-register qreg))
  (setf *current-q-registers* (append *current-q-registers* (list qreg))))

(declare (inline remove-q-register))
(defun remove-q-register (n)
  "Remove a quantum register from the current register list."
  (declare (type fixnum n))
  (setf *current-q-registers* (remove (nth n *current-q-registers*)
				      *current-q-registers*)))

(defun get-nth-q-register (n)
  "Get the nth quantum register in the current register list."
  (declare (type fixnum n))
  (print-quantum-register (nth n *current-q-registers*)))

(declare (inline set-push-opcodes))
(defun set-push-opcodes (state)
  "Set function of *push-opcodes*."
  (declare (type boolean state))
  (setf *push-opcodes* state))

(declare (inline get-push-opcodes))
(defun get-push-opcodes ()
  "Get function of *push-opcodes*."
  *push-opcodes*)
