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
(defclass q-t ())

(defclass q-sequence (q-t)
  ())

(defclass q-array (q-sequence)
  ())

(defclass q-vector (q-array)
  ())

(defclass q-bit-vector (q-vector)
  ())

(defclass q-character (q-t)
  ())

(defclass q-number (q-t)
  ())

(defclass q-complex (q-number)
  ())

(defclass q-float (q-number)
  ())

(defclass q-list (q-sequence)
  ())

(defclass q-cons (q-list)
  ())

(defclass q-function (q-t)
  ())

(defclass q-rational (q-number)
  ())

(defclass q-integer (q-rational)
  ())

(defclass q-symbol (q-t)
  ())

(defclass q-null (q-symbol)
  ())

(defclass q-ratio (q-rational)
  ())

(defclass q-string (q-vector)
  ())

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
