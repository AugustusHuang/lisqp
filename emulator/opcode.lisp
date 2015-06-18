;;;; Opcode manipulation and functions.
;;;; Author: Augustus Huang
;;;; Date: May 25, 2015

(in-package :cl-quantum-emulator)

(defconstant +major-version+ 0)
(defconstant +minor-version+ 1)

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
	 (error "Invalid opcode"))))
