;;;; Packages
;;;; Author: Augustus Huang
;;;; Date: May 22, 2015

(defpackage :general-utilities
  (:nicknames :general-util)
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
  (:use :cl :general-utilities)
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
	   :init-from-file
	   :output-to-file
	   :print-quantum-register
	   :normalize-quantum-register
	   :kronecker-quantum-register
	   :qreg-to-vector
	   :qreg-to-sparse-vector))

(defpackage :cl-quantum-emulator
  (:nicknames :quantum-emulator :qemulator)
  (:use :cl :cl-quantum :general-utilities)
  (:export :emulator
	   :q-+
	   :q-+-mod
	   :q-*-mod
	   :q-expt-mod
	   :qft
	   :inverse-qft))

(defpackage :cl-lisqp
  (:nicknames :lisqp)
  (:use :cl :cl-quantum :cl-quantum-emulator :general-utilities)
  (:export :scan
	   :back-end
	   :top-level
	   :%swap
	   :%grover-algorithm
	   :%shor-algorithm
	   :%linear-system))
