;;;; Lisqp scanner
;;;; Author: Augustus Huang
;;;; Date: May 27, 2015

(in-package :cl-lisqp)

;;; Scanner buffer
(defvar *scan-buffer*)

(defvar *next-to-read*)
(defvar *next-to-write*)

(defvar *scan-base* 10)

(declaim (type integer *next-to-read* *next-to-write*))
(declaim (type (simple-array character (*)) *scan-buffer*))
(declaim (type (integer 2 36) *scan-base*))

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
(defun change-base (new-base)
  "Change the reading number base to 'new'."
  (setf *scan-base* new-base))

(defmacro with-scan-buffer (() &body body)
  `(let* ((*scan-buffer* (make-string 128))
	  (*next-to-read* 0)
	  (*next-to-write* 0))
     ,@body))

(declaim (inline scan-buffer2str))
(defun scan-buffer2str ()
  "Output scan-buffer in string format."
  (subseq *scan-buffer* 0 *next-to-write*))

(declaim (inline plus-p))
(defun plus-p (char)
  (eql char #\+))

(declaim (inline minus-p))
(defun minus-p (char)
  (eql char #\-))

(defun digit-p (char &key (base *scan-base*))
  (declare (type (integer 2 36) base))
  (let* ((code (char-code char)))
    (cond ((and (>= code (char-code #\0))
		(<= code (char-code #\9)))
	   (let ((digit (- code (char-code #\0))))
	     (if (< digit base)
		 digit
		 nil)))
	  ((and (>= code (char-code #\a))
		(<= code (char-code #\z)))
	   (let ((digit (+ 10 (- code (char-code #\a)))))
	     (if (< digit base)
		 digit
		 nil)))
	  ((and (>= code (char-code #\A))
		(<= code (char-code #\Z)))
	   (let ((digit (+ 10 (- code (char-code #\A)))))
	     (if (< digit base)
		 digit
		 nil)))
	  (t
	   nil))))

(defun scan-ratio (token)
  "Scan next token as it was a ratio number."
  (let ((negative-p nil)
	(numerator 0)
	(denominator 0)
	(start 0)
	(end (length token)))
    ()))

(defun scan-int (token)
  "Scan next token as it was an integer."
  (let ((scan-base *scan-base*)
	(negative-p nil)
	(num 0)
	(start 0)
	(end (length token)))
    (progn
      (if (or (plus-p (char token 0))
	      (minus-p (char token 0)))
	  (setf negative-p (minus-p (char token 0))
		start (1+ start)))
      (when (not (= (- end start) 0))
	(do ((offset start (1+ offset)))
	    ((>= offset end)
	     (if negative-p
		 (- num)
		 num))
	  (let ((digit (digit-p (char token offset) :base scan-base)))
	    (when (not digit)
	      (return))
	    (setf num (+ (* num scan-base) digit))))))))
	   
;;; APIs
(defun scan ()
  "Generic scanner routine of the compiler/interpreter."
  )