;;;; Quantum emulator error handlers
;;;; Author: Augustus Huang
;;;; Date: May 29, 2015

(in-package :cl-quantum-emulator)

(define-condition emulator-error (error)
  ()
  (:report "Quantum emulator error."))

(define-condition opcode-error (emulator-error)
  ((opcode-name
    :reader opcode-error-opcode-name
    :initarg :opcode-name))
  (:report (lambda (condition stream)
	     (format stream "Invalid opcode ~A."
		     (opcode-error-opcode-name condition)))))

(define-condition argument-error (emulator-error)
  ((arguments
    :reader argument-error-arguments
    :initarg :arguments))
  (:report (lambda (condition stream)
	     (format stream "Wrong arguments number ~A."
		     (argument-error-arguments condition)))))

(define-condition arithmetic-error (error)
  ((operator
    :reader arithmetic-error-operator
    :initarg :operator)
   (operands
    :reader arithmetic-error-operands
    :initarg :operands
    :initform nil)))

(define-condition division-by-0 (arithmetic-error)
  ())