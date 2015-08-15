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

;;;; Packages

(defpackage :cl-lisqp-utils
  (:nicknames :lisqp-utils)
  (:use :cl)
  (:export :dovec
	   :with-gensyms
	   :make-matrix
	   :make-square-matrix
	   :unitary-matrix-p
	   :identity-matrix-p
	   :trace-zero-p
	   :hermitian-p
	   :matrix-transpose
	   :matrix-conjugate
	   :matrix-adjoint
	   :matrix-*
	   :matrix-+
	   :matrix--
	   :matrix-invert
	   :matrix-determinant
	   :qubits
	   :inner-product
	   :kronecker-product
	   :vec-*-matrix
	   :demoivre
	   :complex-norm
	   :list-dimensions
	   :list-to-array
	   :1d-array-to-list
	   :inverse-mod
	   :make-qreg-with-vector
	   :make-qreg-with-sparse-vector
	   :make-sparse-vector-with-vector))

(defpackage :cl-quantum
  (:nicknames :quantum :cl-qu)
  (:use :cl :cl-lisqp-utils)
  (:export :apply-2-gate
	   :c-not
	   :c-not-ec
	   :hadamard
	   :toffoli
	   :toffoli-ec
	   :swap
	   :phase
	   :pauli-x
	   :pauli-x-ec
	   :pauli-y
	   :pauli-z
	   :pi/8
	   :c-pauli-z
	   :c-phase
	   :fredkin
	   :measure
	   :measure-qubit
	   :grover
	   :oracle
	   :ec-encode
	   :ec-decode
	   :shor
	   :print-quantum-register
	   :normalize-quantum-register
	   :kronecker-quantum-register
	   :qreg-to-vector
	   :qreg-to-sparse-vector
	   :build-q))

(defpackage :cl-quantum-emulator
  (:nicknames :quantum-emulator :cl-qem)
  (:use :cl :cl-quantum :cl-lisqp-utils)
  (:export :quantum-integer
	   :quantum-ratio
	   :quantum-float
	   :q-+
	   :q-+-mod
	   :q-*-mod
	   :q-expt-mod
	   :qft
	   :inverse-qft
	   :opcode-forward
	   :opcode-backward
	   :get-nth-opcode
	   :add-q-register
	   :remove-q-register
	   :get-nth-q-register
	   :input-opcodes
	   :output-opcodes))
