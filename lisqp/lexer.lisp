;;;; Lisqp lexer and parser
;;;; Author: Augustus Huang
;;;; Date: May 25, 2015

(in-package :cl-lisqp)

(deftype form-type ()
  '(member
    :number
    :character
    :symbol
    :function
    :array
    :list
    :string))

(defclass q-form ()
  ((tag
    :type form-type
    :initarg :tag
    :accessor tag
    :documentation "A form's tag")))

(defclass q-number (q-form)
  ((value
    :type number
    :initarg :value
    :accessor value
    :documentation "A number's value")))

(defclass q-character (q-form)
  ((value
    :type character
    :initarg :value
    :accessor value
    :documentation "A character's value")))

(defclass q-symbol (q-form)
  ((name
    :type string
    :initarg :name
    :accessor name
    :documentation "A symbol's name")))

(defclass q-function (q-form)
  ((name
    :type string
    :initarg :name
    :accessor name
    :documentation "A function's name")))

(defclass q-list (q-form)
  ((head
    :type q-form
    :initarg :head
    :accessor head
    :documentation "A list's head")
   (tail
    :type q-form
    :initarg :tail
    :accessor tail
    :documentation "A list's tail")))

(defclass q-array (q-form)
  ((values
    :type array
    :initarg :values
    :accessor values
    :documentations "An array's values")))

(defclass q-string (q-form)
  ((value
    :type string
    :initarg :value
    :accessor value
    :documentations "A string's value")))

(defgeneric form2str (form)
  (:documentation "Make a string by current form."))

(defmethod form2str (form q-number)
  (format nil "~A" (value form)))

(defmethod form2str (form q-character)
  (format nil "~A" (value form)))

(defmethod form2str (form q-symbol)
  (format nil "~A" (name form)))

(defmethod form2str (form q-function)
  (format nil "~A" (name form)))

;;; FIXME
(defmethod form2str (form q-list)
  (format nil "~A ~A" (head form) (tail form)))

;;; FIXME
(defmethod form2str (form q-array)
  (format nil "~A" (values form)))

(defmethod form2str (form q-string)
  (format nil (value form)))

;;; APIs
(defun lex-parse ())