a;;;; Lisqp scanner
;;;; Author: Augustus Huang
;;;; Date: May 27, 2015

(in-package :cl-lisqp)

;;; Scanner buffer
(defvar *scan-buffer*)

(defvar *next-to-read*)
(defvar *next-to-write*)

(defvar *scan-base* 10)

(defstruct (scan-table
	     (:predicate scan-table-p)
	     (:copier))
  (case :upcase :type (member :upcase :downcase :preserve :invert))
  (base-characters (make-array 256 :initial-element nil) :type (simple-vector 256))
  (extended-characters (make-hash-table) :type hash-table))

(defun copy-scan-table (&optional (from-table *scan-table*) to-table)
  (when (eql to-table 'nil)
    (setf to-table (make-scan-table)))
  (when (eql from-table 'nil)
    (setf from-table *standard-scan-table*))
  (setf (scan-table-case to-table) (scan-table-case from-table))
  (dotimes (i 256)
    (set-syntax-from-char (code-char i) (code-char i) to-table from-table))
  (clrhash (scan-table-extended-characters to-table))
  (maphash (lambda (k v)
	     (declare (ignore))
	     (set-syntax-from-char k k to-table from-table))
	   (scan-table-extended-characters from-table))
  to-table)
a
(defun scan-table-syntax-type (char &optional (scan-table *scan-table*))
  (unless scan-table
    (setf scan-table *standard-scan-table*))
  (cond ((base-char-p char)
	 (svref (scan-table-base-characters scan-table) (char-code char)))
	(t
	 (gethash char (scan-table-extended-characters scan-table) nil))))

(defun (setf scan-table-syntax-type) (value char &optional (scan-table *scan-table*))
  (unless scan-table
    (setf scan-table *standard-scan-table*))
  (cond ((base-char-p char)
	 (setf (svref (scan-table-base-characters scan-table) (char-code char)) value))
	(t
	 (setf (gethash char (scan-table-extended-characters scan-table)) value))))

(defun get-scan-macro-character (char &optional (scan-table *scan-table*))
  (let ((data (scan-table-syntax-type char scan-table)))
    (if (listp data)
	(values (first data) (second data))
	(values nil nil))))

(defun set-scan-macro-character (char new-function &optional non-terminating-p (scan-table *scan-table*))
  (if new-function
      (setf (scan-table-syntax-type char scan-table)
	    (list new-function (not non-terminating-p)))
      (setf (scan-table-syntax-type char scan-table) nil))
  t)

(defun make-scan-dispatch-macro-character (char &optional non-terminating-p (scan-table *scan-table*))
  (setf (scan-table-syntax-type char scan-table)
	(list 'read-dispatch-char (not non-terminating-p)
	      (make-array 256 :initial-element nil)
	      (make-hash-table)))
  t)

(defun get-scan-dispatch-macro-character (disp-char sub-char &optional (scan-table *scan-table*))
  (let ((data (scan-table-syntax-type disp-char scan-table)))
    (unless (and (listp data) (= (length data) 4))
      (error "Character ~S is not a dispatching macro character." disp-char))
    (cond ((base-char-p sub-char)
	   (svref (third data) (char-code sub-char)))
	  (t (gethash sub-char (fourth data))))))

(defun set-scan-dispatch-macro-character (disp-char sub-char new-function &optional (scan-table *scan-table*))
  (setf sub-char (char-upcase sub-char))
  (let ((data (scan-table-syntax-type disp-char scan-table)))
    (unless (and (listp data) (= (length data) 4))
      (error "Character ~S is not a dispatching macro character." disp-char))
    (cond ((base-char-p sub-char)
	   (setf (svref (third data) (char-code sub-char)) new-function))
	  (t (setf (gethash sub-char (fourth data)) new-function))))
  t)

(defun set-syntax-from-char (to-char from-char &optional (to-table *scan-table*) from-table)
  (let ((data (scan-table-syntax-type from-char from-table)))
    (cond ((not (consp data))
	   (setf (scan-table-syntax-type to-char to-table) data))
	  ((= (length data) 4)
	   ;; Dispatching character, must copy the dispatch tables.
	   (let ((ht (make-hash-table)))
	     (maphash (lambda (k v) (setf (gethash k ht) v)) (fourth data))
	     (setf (scan-table-syntax-type to-char to-table)
		   (list (first data) (second data)
			 (make-array 256 :initial-contents (third data))
			 ht))))
	  (t (setf (scan-table-syntax-type to-char to-table)
		   (copy-list data)))))
  t)

(defun case-correct (c &optional (rt *readtable*))
  "Change the case of C depending on the readtable-case of RT."
  (ecase (readtable-case rt)
    (:upcase (char-upcase c))
    (:downcase (char-downcase c))
    (:preserve c)
    (:invert (if (upper-case-p c)
		 (char-downcase c)
		 (char-upcase c)))))

;;; Scanner readtable
(declaim (special *scan-table*)
	 (type scan-table *scan-table*))
;;; There will be some differences between lisqp and lisp!
(defvar *scan-table* (copy-scan-table))

(declaim (type integer *next-to-read* *next-to-write*))
(declaim (type (simple-array character (*)) *scan-buffer*))
(declaim (type (integer 2 36) *scan-base*))

(setf (scan-table-syntax-type #\Tab *scan-table*) :whitespace
      (scan-table-syntax-type #\Newline *scan-table*) :whitespace
      (scan-table-syntax-type #\Linefeed *scan-table*) :whitespace
      (scan-table-syntax-type #\Page *scan-table*) :whitespace
      (scan-table-syntax-type #\Return *scan-table*) :whitespace
      (scan-table-syntax-type #\Space *scan-table*) :whitespace
      (scan-table-syntax-type #\\ *scan-table*) :single-escape
      (scan-table-syntax-type #\| *scan-table*) :multiple-escape)

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
  `(let* ((*scan-buffer* (make-string 32))
	  (*next-to-read* 0)
	  (*next-to-write* 0))
     ,@body))

(declaim (inline scan-buffer2str))
(defun scan-buffer2str ()
  "Output scan-buffer in string format."
  (subseq *scan-buffer* 0 *next-to-write*))

(declaim (inline base-char-p))
(defun base-char-p (char)
  (and (characterp char)
       (< (char-code char) 255)))

(declaim (inline plus-p))
(defun plus-p (char)
  (eql char #\+))

(declaim (inline minus-p))
(defun minus-p (char)
  (eql char #\-))

(declaim (inline whitespace[2]p))
(defun whitespace[2]p (char &optional (scan-table *scan-table*))
  "Test if 'char' is a whitespace[2] character in 'scan-table."
  (eql (scan-table-syntax-type char scan-table) :whitespace))

(declaim (inline invalidp))
(defun invalidp (char &optional (scan-table *scan-table*))
  "Test if 'char' is an invalid character in 'scan-table'."
  (and (eql (scan-table-syntax-type char scan-table) nil)
       (or (member char '(#\Backspace #\Tab #\Newline #\Linefeed #\Page
			  #\Return #\Space #\Rubout)))))

(defun terminating-macro-p (char &optional (scan-table *scan-table*))
  (multiple-value-bind (fn terminatingp)
      (get-scan-macro-character char scan-table)
    (declare (ignore fn))
    terminatingp))

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
;;; Don't evaluate to themselves but make qubits!
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
  "Scan a token from 'stream' with 'first' as the initial character."
  ;; Maybe use with-scan-buffer.
  (let ((token (make-array 16 :element-type 'character
			   :adjustable t
			   :fill-pointer 0))
	(seen-escape nil))
    (do ((x first (read-char stream nil 'nil t)))
	;; Read characters until EOF or a whitespace character or terminating macro character is seen.
	((or (eql x 'nil)
	     (when (or (terminating-macro-p x) (whitespace[2]p x))
	       (unread-char x stream)
	       t)))
      (let ((syntax (scan-table-syntax-type x)))
	(cond ((eql syntax :single-escape)
	       ;; Single escape character, read the next character directly
	       ;; and do not try to parse the token as a number.
	       (let ((y (read-char stream t nil t)))
		 (setf seen-escape t)
		 (vector-push-extend y token)))
	      ((eql syntax :multiple-escape)
	       ;; Multiple escape character, read characters until another
	       ;; multiple escape character is seen.
	       ;; Treat single escape characters as above.
	       ;; Don't parse the token as a number.
	       (setf seen-escape t)
	       (do ((y (read-char stream t nil t)
		       (read-char stream t nil t)))
		   ((eql (scan-table-syntax-type y) :multiple-escape))
		 (if (eql (scan-table-syntax-type y) :single-escape)
		     (vector-push-extend (read-char stream t nil t) token)
		     (vector-push-extend y token))))
	      (t (vector-push-extend (case-correct x) token)))))
    ;; Check for invalid uses of the dot. Tokens that are constructed
    ;; entirely from dots are invalid unless one or more of the dots was
    ;; escaped or a package name was explicitly specified.
    (unless seen-escape
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
      (seen-escape
       (intern token))
      (t (or (read-integer token)
	     (read-ratio token)
             (intern token))))))

(defun scan-left-parenthesis (stream ignore)
  "Scan left parenthesis and match its partner."
  (declare (ignore ignore))
  ())

(defun scan-right-parenthesis (stream first)
  "Scan a right parenthesis when mismatched."
  (declare (ignore stream))
  (error 'scanner-error
	 :stream stream
	 :format-control "Unexpected ~S."
	 :format-arguments (list first)))

(defun scan-comment (stream ignore)
  "Scan and pass over comment."
  (declare (ignore ignore))
  (do () ((char= (read-char stream nil #\Newline t) #\Newline)))
  (values))

(defun scan-quote (stream ignore)
  "Scan quoted list."
  (declare (ignore ignore))
  (list 'quote (read stream t nil t)))

(defun scan-classic-function (stream sub c)
  "Scan a function, classic version."
  (declare (ignore sub c))
  (list 'function (scan stream t nil t)))

(defun scan-quantum-function (stream sub c)
  "Scan a function, quantum version."
  (declare (ignore sub c))
  (list 'q-function (scan stream t nil t)))

(defun scan-character (stream sub c)
  "Scan a character."
  )

(defun scan-string (stream ignore)
  "Scan a string."
  (declare (ignore ignore))
  )

(defun scan-radix (stream sub c)
  "Scan a different based number."
  (let* ((bases '((#\B . 2) (#\O . 8) (#\X . 16)))
	 (usual-base (assoc sub bases))
	 (base (cond (usual-base
		      (cdr usual-base))
		     (c c)
		     (t
		      (error "Invalid radix #~A" sub)))))
    (declare (type (integer 2 36) base))
    (let ((*scan-base* base))
      (the rational (scan stream t nil t)))))

(defun scan-complex (stream sub c)
  "Scan a complex number."
  (declare (ignore sub c))
  (let ((num (scan stream t nil t)))
    (when (or (not (listp num))
	      (/= (length num) 2)
	      (not (realp (first num)))
	      (not (realp (second num))))
      (error "Invalid complex number ~S" num))
    (complex (first num) (second num))))

(defun scan-array (stream sub c)
  "Scan an array."
  )

(defun scan-big-comment (stream sub c)
  "Scan a big comment surrounded with '#|' and '|#'."
  )

(defun scan-intern (stream eof-error-p eof-value recursive-p)
  (loop (let ((c (read-char stream eof-error-p 'nil t)))
	  (when (eql c 'nil)
	    (return eof-value))
	  (when (invalidp c)
	    (error 'scanner-error :stream stream
		   :format-control "Read invalid character ~S."
		   :format-arguments (list c)))
	  (unless (whitespace[2]p c)
	    (let ((value (multiple-value-list (funcall (or (get-macro-character c)
							   #'read-token)
						       stream c))))
	      (when value
		(return (first value))))))))

;;; APIs
;;; Missing: 'follow-stream-designator 'with-stream-editor
(defun scan (&optional stream (eof-error-p t) eof-value recursive-p)
  "Generic scan routine of the compiler/interpreter."
  (let ((s (follow-stream-designator stream *standard-input*)))
    (with-stream-editor (s recursive-p)
      (let ((result (scan-intern s eof-error-p eof-value recursive-p)))
	(unless (or (eql result eof-value)
		    recursive-p)
	  (let ((c (read-char s nil nil)))
	    (when (and c
		       (not (whitespace[2]p c)))
	      (unread-char c s))))
	result))))

(progn
  (set-scan-macro-character #\( 'scan-left-parenthesis nil *scan-table*)
  (set-scan-macro-character #\) 'scan-right-parenthesis nil *scan-table*)
  (set-scan-macro-character #\' 'scan-quote nil *scan-table*)
  (set-scan-macro-character #\; 'scan-comment nil *scan-table*)
  (set-scan-macro-character #\" 'scan-string nil *scan-table*)
  (make-scan-dispatch-macro-character #\# t *scan-table*)
  (set-scan-dispatch-macro-character #\# #\' 'scan-classic-function *scan-table*)
  (set-scan-dispatch-macro-character #\# #\B 'scan-radix *scan-table*)
  (set-scan-dispatch-macro-character #\# #\O 'scan-radix *scan-table*)
  (set-scan-dispatch-macro-character #\# #\X 'scan-radix *scan-table*)
  (set-scan-dispatch-macro-character #\# #\R 'scan-radix *scan-table*)
  (set-scan-dispatch-macro-character #\# #\C 'scan-complex *scan-table*)
  (set-scan-dispatch-macro-character #\# #\A 'scan-array *scan-table*)
  (set-scan-dispatch-macro-character #\# #\| 'scan-big-comment *scan-table*)
  (make-scan-dispatch-macro-character #\% t *scan-table*)
  (set-scan-dispatch-macro-character #\% #\' 'scan-quantum-function *scan-table*))