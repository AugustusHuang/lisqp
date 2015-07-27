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
