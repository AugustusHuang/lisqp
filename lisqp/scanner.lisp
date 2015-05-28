;;;; Lisqp scanner
;;;; Author: Augustus Huang
;;;; Date: May 27, 2015

(in-package :cl-lisqp)

;;; Scanner buffer
(defvar *scan-buffer*)

(defvar *next-to-read*)
(defvar *next-to-write*)

(defvar *number-base* 10)

(declaim (type integer *next-to-read* *next-to-write*))
(declaim (type (simple-array character (*)) *scan-buffer*))
(declaim (type (integer 2 36) *number-base*))

(declaim (inline reset-scan-buffer))
(defun reset-scan-buffer ()
  "Empty scan-buffer, and reset next-to pointers."
  (setf *next-to-read* 0
	*next-to-write* 0))

(declaim (inline reset-ntr))
(defun reset-ntr ()
  "Reset ntr pointer to 0."
  (setf *next-to-read* 0))

(declaim (inline ntr-read))
(defun ntr-read ()
  "Read character pointed by ntr."
  (if (>= *next-to-read* *next-to-write*)
      *eof-object*
      (prog1
	  (elt *scan-buffer* *next-to-read*)
	(incf *next-to-read*))))

(declaim (inline ntw-read))
(defun ntw-read (char)
  "Read character pointed by ntw."
  (when (>= *next-to-write* *scan-buffer*)
    (double-buffer))
  (setf (elt *scan-buffer* *next-to-write*) char
	*next-to-write* (1+ *next-to-write*)))

(declaim (inline unread))
(defun unread ()
  "Puke a character."
  (decf *next-to-read*))

(declaim (inline double-buffer))
(defun double-buffer ()
  "Double the buffer size."
  (let* ((bl (length *scan-buffer*))
	 (new-bl (* bl 2))
	 (new-buffer (make-string new-bl)))
    (setf *scan-buffer* (replace new-buffer *scan-buffer*))))

(declaim (inline change-base))
(defun change-base (new)
  "Change the reading number base to 'new'."
  (setf *number-base* new))

(defmacro with-scan-buffer (() &body body)
  `(let* ((*scan-buffer* (make-string 128))
	  (*next-to-read* 0)
	  (*next-to-write* 0))
     ,@body))

(declaim (inline scan-buffer2str))
(defun scan-buffer2str ()
  "Output scan-buffer in string format."
  (subseq *scan-buffer* 0 *next-to-write*))

;;; APIs
(defun scan ()
  "Generic scanner routine of the compiler/interpreter."
  )