;;;; Compiler/Interpreter errors
;;;; Author: Augustus Huang
;;;; Date: May 29, 2015

(in-package :cl-lisqp)

;;; Error handlers dealing with top-level will be called in classical content,
;;; while emulator error handlers will be present in emulator files and called
;;; in quantum content.
(define-condition array-dimension-error (error)
  ((array :initarg :array
	  :reader array-dimension-error-array)
   (dimension :initarg :dimension
	      :read array-dimension-error-dimension)))

(define-condition arithmetic-error (error)
  ((operator :initarg :operator
	     :reader arithmetic-error-operator)
   (operands :initarg :operands
	     :initform nil
	     :reader arithmetic-error-operands)))

;;; When we are entering a naive ratio with denominator 0.
(define-condition division-by-0 (arithmetic-error)
  ())

(define-condition stream-error (error)
  ((stream :initarg :stream
	   :reader stream-error-stream)))

(define-condition eof-error (stream-error)
  ())

(define-condition scanner-error (stream-error lexer-parser-error)
  ())

(define-condition lexer-parser-error (error)
  ())

(define-condition symbol-error (error)
  ((name :initarg :name
	 :reader symbol-error-name)))

(define-condition unbound-error (symbol-error)
  ()
  (:report (lambda (sym stream)
	     (format stream "Variable ~S is unbounded." (symbol-error-name sym)))))

(define-condition undefined-error (symbol-error)
  ()
  (:report (lambda (sym stream)
	     (format stream "Function ~S undefined." (symbol-error-name sym)))))