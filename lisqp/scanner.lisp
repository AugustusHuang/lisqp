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

(declaim (inline unscan))
(defun unscan ()
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

;;; With reversible computations, it seems that there's no need to introduce
;;; floating point numbers --- May 29, 2015, Augustus
(defun scan-ratio (token)
  "Scan next token as it was a ratio number."
  (let ((negative-p nil)
	(numerator 0)
	(denominator 0)
	(start 0)
	(end (length token))
	(in-denominator-p nil))
    (if (or (plus-p (char token 0))
	    (minus-p (char token 0)))
	(setf negative-p (minus-p (char token 0))
	      start (1+ start)))
    (when (not (= (- end start) 0))
      (do ((offset start (1+ offset)))
	  ((>= offset end)
	   (if negative-p
	       (- (/ numerator denominator))
	       (/ numerator denominator)))
	(let* ((point (char token offset))
	       (digit (digit-p point :base 10)))
	  (when (and (not digit)
		     (char/= point #\/))
	    (return-from scan-ratio))
	  (cond ((and digit in-denominator-p)
		 (setf denominator (+ (* denominator 10) digit)))
		((and digit (not in-denominator-p))
		 (setf numerator (+ (* numerator 10) digit)))
		((and (char= point #\/)
		      (not in-denominator-p))
		 (setf in-denominator-p t))
		(t
		 (return-from scan-ratio)))))))))

(defun scan-int (token)
  "Scan next token as it was an integer."
  (let ((scan-base *scan-base*)
	(negative-p nil)
	(num 0)
	(start 0)
	(end (length token)))
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
	    (return-from scan-int))
	  (setf num (+ (* num scan-base) digit))))))))

;;; FIXME: constructing
(defun scan-token (stream first)
  "scan a token from 'stream' with 'first' as the initial character."
  ;; Maybe use with-scan-buffer.
  (let ((token (make-array 16 :element-type 'character
			   :adjustable t
			   :fill-pointer 0))
	(seen-escape nil)
	(package-name nil)
	(intern-symbol nil))
    (do ((x first (read-char stream nil 'nil t)))
	;; Read characters until EOF or a whitespace character or terminating macro character is seen.
	((or (eql x 'nil)
	     (when (or (terminating-macro-p x) (whitespace[2]p x))
	       (unread-char x stream)
	       t)))
      (let ((syntax (readtable-syntax-type x)))
	(cond ((eql syntax :single-escape)
	       ;; Single escape character, read the next character directly
	       ;; and do not try to parse the token as a number.
	       (let ((y (read-char stream t nil t)))
		 (setf seen-escape t)
		 (vector-push-extend y token)))
	      ((eql syntax :multiple-escape)
	       ;; Multiple escape character, read characters until another multiple escape character
	       ;; is seen. Treat single escape characters as above. Don't parse the token as a number.
	       (setf seen-escape t)
	       (do ((y (read-char stream t nil t)
		       (read-char stream t nil t)))
		   ((eql (readtable-syntax-type y) :multiple-escape))
		 (if (eql (readtable-syntax-type y) :single-escape)
		     (vector-push-extend (read-char stream t nil t) token)
		     (vector-push-extend y token))))
	      ((eql x #\:)
	       ;; Treat the token that was read in as a package name.
	       (when package-name
		 (error 'simple-reader-error :stream stream
			:format-control "Too many package markers."
			:format-arguments '()))
	       ;; Test for "::" and invalid uses of the package marker
	       (let ((y (read-char stream t nil t)))
		 (when (or (terminating-macro-p y) (whitespace[2]p y))
		   (error 'simple-reader-error :stream stream
			  :format-control "Invalid character ~S following package marker."
			  :format-arguments (list y)))
		 (if (eql y #\:)
		     (setf intern-symbol t)
		     (unread-char y stream))
		 ;; ":" and "::" with no leading package name denotes the KEYWORD package.
		 (if (and (not seen-escape) (= 0 (length token)))
		     (setf package-name "KEYWORD")
		     (setf package-name token
			   token (make-array 16
					     :element-type 'character
					     :adjustable t
					     :fill-pointer 0)))))
	      (t (vector-push-extend (case-correct x) token)))))
    ;; Check for invalid uses of the dot. Tokens that are constructed
    ;; entirely from dots are invalid unless one or more of the dots was
    ;; escaped or a package name was explicitly specified.
    (unless (or package-name seen-escape)
      (do ((offset 0 (1+ offset))
	   (dot-count 0))
	  ((>= offset (fill-pointer token))
	   (when (= dot-count (fill-pointer token))
	     (error 'simple-reader-error :stream stream
		    :format-control "Token ~S only contains dots."
		    :format-arguments (list token))))
	(when (eql (char token offset) #\.)
	  (incf dot-count))))
    (cond
      ;; Return a symbol immediately if a package marker was seen.
      (package-name
       (if (or intern-symbol (string= "KEYWORD" package-name))
	   (intern token package-name)
	   (multiple-value-bind (symbol status) (find-symbol token package-name)
	     (unless (eql status :external)
	       (error 'simple-reader-error :stream stream
		      :format-control "Symbol ~S is internal to package ~S."
		      :format-arguments (list token package-name)))
	     symbol)))
      (seen-escape
       (intern token))
      (t (or (read-integer token)
	     (read-ratio token)
             (intern token))))))

(defun scan-left-parenthesis (stream ignore)
  (declare (ignore ignore))
  ())

(defun scan-right-parenthesis (stream first)
  (declare (ignore stream))
  (error 'scanner-error
	 :stream stream
	 :format-control "Unexpected ~S."
	 :format-arguments (list first)))

(defun scan-comment (stream ignore)
  (declare (ignore ignore))
  ())

(defun scan-quote (stream ignore)
  (declare (ignore ignore))
  ;; Classical
  (list 'quote (read stream t nil t)))

(defun scan-function (stream ignore)
  (declare (ignore ignore))
  )

(defun scan-character (stream)
  )

(defun scan-radix (stream)
  )

(defun scan-complex (stream)
  )

(defun scan-array (stream)
  )

;;; APIs
(defun scan ()
  "Generic scanner routine of the compiler/interpreter."
  )