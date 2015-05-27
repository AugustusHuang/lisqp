;;;; Error correction code
;;;; Author: Augustus Huang
;;;; Date: May 25, 2015

(in-package :cl-quantum)

(defparameter *ec-width* 0)

(declaim (inline set-ec-width))
(defun set-ec-width (width)
  "Set error correction protected width."
  (setf *ec-width* width))

(declaim (inline get-ec-width))
(defun get-ec-width ()
  "Get error correction protected width."
  *ec-width*)