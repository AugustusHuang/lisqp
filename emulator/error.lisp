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

;;;; Quantum emulator error handlers
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
