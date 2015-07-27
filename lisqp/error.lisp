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

;;;; Compiler/Interpreter errors
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
