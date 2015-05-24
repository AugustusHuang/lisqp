;;;; Packages
;;;; Author: Augustus Huang
;;;; Date: May 22, 2015

(defpackage :general-utilities
  (:nicknames :general-util)
  (:use :cl)
  (:export :dovec
	   :with-gensyms
	   :transpose
	   :conjugate
	   :adjoint
	   :matrix-*
	   :matrix-+
	   :matrix--))

(defpackage :cl-quantum
  (:nicknames :quantum :cl-qu)
  (:use :cl :general-utilities)
  (:export :c-not
	   :hadamard
	   :toffoli
	   :swap
	   :phase
	   :pauli-x
	   :pauli-y
	   :pauli-z
	   :pi/8
	   :c-pauli-z
	   :c-phase
	   :fredkin
	   :measure
	   :init-qubit
	   :init-from-file
	   :output-to-file))

(defpackage :cl-quantum-emulator
  (:nicknames :quantum-emulator :qemulator)
  (:use :cl :cl-quantum :general-utilities)
  (:export :emulator))

(defpackage :cl-lisqp
  (:nicknames :lisqp)
  (:use :cl :cl-quantum :cl-quantum-emulator :general-utilities)
  (:export :lexer
	   :parser
	   :back-end
	   :top-level))