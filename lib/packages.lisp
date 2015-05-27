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
	   :init-qubit
	   :init-from-file
	   :output-to-file))

(defpackage :cl-quantum-emulator
  (:nicknames :quantum-emulator :qemulator)
  (:use :cl :cl-quantum :general-utilities)
  (:export :emulator
	   :q+
	   :q+mod
	   :q*mod
	   :q-expr-mod))

(defpackage :cl-lisqp
  (:nicknames :lisqp)
  (:use :cl :cl-quantum :cl-quantum-emulator :general-utilities)
  (:export :lex-parse
	   :back-end
	   :top-level
	   :defcategory))