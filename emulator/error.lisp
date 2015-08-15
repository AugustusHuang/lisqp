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

;;;; Quantum emulator error handlers

(in-package :cl-quantum-emulator)

(define-condition emulator-error (error)
  ()
  (:report "Quantum emulator error."))

(define-condition opcode-error (emulator-error)
  ((opcode-name
    :reader opcode-error-opcode-name
    :initarg :opcode-name)
   (opcode-args
    :reader opcode-error-opcode-args
    :initarg :opcode-args))
  (:report (lambda (condition stream)
	     (format stream "Invalid opcode ~A with arguments ~A."
		     (opcode-error-opcode-name condition)
		     (opcode-error-opcode-args condition)))))

(define-condition opcode-pointer-error (emulator-error)
  ((step
    :reader opcode-pointer-error-step
    :initarg :step)
   (current
    :reader opcode-pointer-error-current
    :initarg :current))
  (:report (lambda (condition stream)
	     (format stream "Try to move opcode pointer ~D steps backward but opcode pointer now is ~D."
		     (opcode-pointer-error-step condition)
		     (opcode-pointer-error-current condition)))))

(define-condition argument-error (emulator-error)
  ((arguments
    :reader argument-error-arguments
    :initarg :arguments))
  (:report (lambda (condition stream)
	     (format stream "Wrong arguments number ~A."
		     (argument-error-arguments condition)))))

(define-condition quantum-arithmetic-error (emulator-error)
  ((operator
    :reader quantum-arithmetic-error-operator
    :initarg :operator)
   (operands
    :reader quantum-arithmetic-error-operands
    :initarg :operands))
  (:report (lambda (condition stream)
	     (format stream "Arithmetic error ~A with operands ~A."
		     (quantum-arithmetic-error-operator condition)
		     (quantum-arithmetic-error-operands condition)))))

(define-condition division-by-0-error (arithmetic-error)
  ()
  (:report "Division by 0."))
