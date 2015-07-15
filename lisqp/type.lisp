;;;; Lisqp types, type maps and routines
;;;; Author: Augustus Huang
;;;; Date: May 29, 2015

(in-package :cl-lisqp)

(deftype internal-types ()
  '(member
    ;; (array sequence t)
    :q-array
    ;; (bit-vector vector array sequence t)
    :q-bit-vector
    ;; (character t)
    :q-character
    ;; (complex number t)
    :q-complex
    ;; (cons list sequence t)
    :q-cons
    ;; (float number t)
    :q-float
    ;; (function t)
    :q-function
    ;; (integer rational number t)
    :q-integer
    ;; (list sequence t)
    :q-list
    ;; (null symbol list sequence t)
    :q-null
    ;; (number t)
    :q-number
    ;; (random-state t)
    :q-random-state
    ;; (ratio rational number t)
    :q-ratio
    ;; (rational number t)
    :q-rational
    ;; (sequence t)
    :q-sequence
    ;; (string vector array sequence t)
    :q-string
    ;; (symbol t)
    :q-symbol
    ;; (t)
    :q-t
    ;; (vector array sequence t)
    :q-vector))

;;; Implement type hierarchy, all those classic evaluation will be replaced
;;; by quantum register usage and evaluation.
;;; Some of them are only informational classes, they don't provide any
;;; implementation but present.
(defclass q-t ())

;;; FIXME: Sequence should have no implementation?
(defclass q-sequence (q-t) ())

;;; Array will be actually vector and with dimensions information.
(defclass q-array (q-sequence)
  ((qarray :type (vector (quantum-register)))
   (qdim :type list)))

;;; And so vector will be array, they are equal.
(defclass q-vector (q-array) ())

;;; Bit vector will be supported here?
(defclass q-bit-vector (q-vector) ())

;;; A char is a quantum register escaped or not.
(defclass q-character (q-t)
  ((qchar :type quantum-register)
   (qescaped :type boolean)))

;;; A number contained in a quantum register. We view it as a general number.
;;; So it will have no base or no counting system.
;;; We can't tell the difference between int|001> and float|0.01>,
;;; though they seem the same when viewed as a total quantum register.
(defclass q-number (q-t)
  ((qnum :type quantum-register)))

;;; Introducing the imaginary part, and complex numbers are present.
(defclass q-complex (q-number)
  ((qimag :type quantum-register)))

;;; Introducing the point, and floating point numbers are present.
(defclass q-float (q-number)
  ((qpoint :type fixnum)))

;;; List is different, it should have a tail pointer.
(defclass q-list (q-sequence)
  ((qhead :type quantum-register)
   (qtail :type q-list)))

;;; FIXME: I don't know now.
(defclass q-cons (q-list) ())

;;; Ah a quantum function will be no different than a classic one.
(defclass q-function (q-t)
  ((name :type string)
   (args :type list)))

;;; All we can store is rational...
(defclass q-rational (q-number) ())

;;; Integer is the default type.
(defclass q-integer (q-rational) ())

;;; Classic symbols = quantum symbols.
(defclass q-symbol (q-t)
  ((name :type string)
   (value :type q-t)))

(defclass q-null (q-symbol)
  ((value :type null)))

;;; a = a/1
(defclass q-ratio (q-rational)
  ((qdenom :type q-integer)))

(defclass q-string (q-vector)
  ((qescaped :type boolean)))

(defgeneric make-basic-type (type)
  (:documentation "Initialize a new basic type data structure."))

(defmethod make-basic-type (type q-t)
  (%make-instance 'q-t))

(defmethod make-basic-type (type q-sequence)
  (%make-instance 'q-sequence))

(defmethod make-basic-type (type q-array)
  (%make-instance 'q-array))

(defmethod make-basic-type (type q-vector)
  (%make-instance 'q-vector))

(defmethod make-basic-type (type q-bit-vector)
  (%make-instance 'q-bit-vector))

(defmethod make-basic-type (type q-character)
  (%make-instance 'q-character))

(defmethod make-basic-type (type q-number)
  (%make-instance 'q-number))

(defmethod make-basic-type (type q-complex)
  (%make-instance 'q-complex))

(defmethod make-basic-type (type q-list)
  (%make-instance 'q-list))

(defmethod make-basic-type (type q-cons)
  (%make-instance 'q-cons))

(defmethod make-basic-type (type q-function)
  (%make-instance 'q-function))

(defmethod make-basic-type (type q-rational)
  (%make-instance 'q-rational))

(defmethod make-basic-type (type q-integer)
  (%make-instance 'q-integer))

(defmethod make-basic-type (type q-symbol)
  (%make-instance 'q-symbol))

(defmethod make-basic-type (type q-null)
  (%make-instance 'q-null))

(defmethod make-basic-type (type q-ratio)
  (%make-instance 'q-ratio))

(defmethod make-basic-type (type q-string)
  (%make-instance 'q-string))

(defmethod dimensions (type q-array)
  (slot-value q-array dimensions))

(defmethod dimensions (type q-vector)
  (slot-value q-vector length))

(defmethod dimensions (type q-list)
  )
