(in-package :cl-quantum-emulator)

(define-condition emulator-error (error) ()
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