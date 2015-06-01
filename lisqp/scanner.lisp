;;;; Lisqp scanner
;;;; Author: Augustus Huang
;;;; Date: May 27, 2015

(in-package :cl-lisqp)

;;; Scanner buffer
(defvar *scan-buffer*)

(defvar *next-to-read*)
(defvar *next-to-write*)

(defvar *scan-base* 10)

;;; Scanner readtable
(declaim (special *original-table* *scan-table*)
	 (type readtable *original-table* *scan-table*))
;;; There will be some differences between lisqp and lisp!
(defvar *scan-table* (copy-readtable))
;;; This is for recovery.
(defvar *original-table* (copy-readtable nil))

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
  `(let* ((*scan-buffer* (make-string 32))
	  (*next-to-read* 0)
	  (*next-to-write* 0))
     ,@body))

(declaim (inline scan-buffer2str))
(defun scan-buffer2str ()
  "Output scan-buffer in string format."
  (subseq *scan-buffer* 0 *next-to-write*))

;;; Now I assume ASCII here.
;;; TODO: How about Unicode?
(declaim (inline base-char-p))
(defun base-char-p (char)
  (and (characterp char)
       (< (char-code char) 127)))

(declaim (inline plus-p))
(defun plus-p (char)
  (eql char #\+))

(declaim (inline minus-p))
(defun minus-p (char)
  (eql char #\-))

;;; Maybe I should implement a scan-table structure???
(declaim (inline whitespace[2]p))
(defun whitespace[2]p (char &optional (readtable *readtable*))
  "Test if CHAR is a whitespace[2] character under READTABLE."
  (eql (readtable-syntax-type char readtable) :whitespace))

(declaim (inline invalidp))
(defun invalidp (char &optional (readtable *readtable*))
  "Test if CHAR is an invalid character under READTABLE."
  (and (eql (readtable-syntax-type char readtable) nil)
       (or (member char '(#\Backspace #\Tab #\Newline #\Linefeed #\Page
			  #\Return #\Space #\Rubout)))))

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

(defun readtable-syntax-type (char &optional (readtable *readtable*))
  (check-type readtable (or readtable null) "a readtable designator")
  (check-type char character)
  (unless readtable
    (setf readtable *standard-readtable*))
  (cond ((base-char-p char)
	 (svref (readtable-base-characters readtable) (char-code char)))
	(t
	 (gethash char (readtable-extended-characters readtable) nil))))

(defun (setf readtable-syntax-type) (value char &optional (readtable *readtable*))
  (check-type readtable (or readtable null) "a readtable designator")
  (check-type char character)
  (unless readtable
    (setf readtable *standard-readtable*))
  (cond ((base-char-p char)
	 (setf (svref (readtable-base-characters readtable) (char-code char)) value))
	(t
	 (setf (gethash char (readtable-extended-characters readtable)) value))))

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

;;; Setup
;;; To use *scan-table* as the default readtable,
;;; use (setq *readtable* (copy-readtable *scan-table*)) after initialization.
;;; To restore standard common lisp syntax,
;;; use (setq *readtable* (copy-readtable nil)).
(progn
  (set-macro-character #\( 'scan-left-parenthesis nil *scan-table*)
  (set-macro-character #\) 'scan-right-parenthesis nil *scan-table*)
  (set-macro-character #\' 'scan-quote nil *scan-table*)
  (set-macro-character #\; 'scan-comment nil *scan-table*)
  (set-macro-character #\" 'scan-string nil *scan-table*)
  (make-dispatch-macro-character #\# t *scan-table*)
  (set-dispatch-macro-character #\# #\' 'scan-classic-function *scan-table*)
  (set-dispatch-macro-character #\# #\B 'scan-radix *scan-table*)
  (set-dispatch-macro-character #\# #\O 'scan-radix *scan-table*)
  (set-dispatch-macro-character #\# #\X 'scan-radix *scan-table*)
  (set-dispatch-macro-character #\# #\R 'scan-radix *scan-table*)
  (set-dispatch-macro-character #\# #\C 'scan-complex *scan-table*)
  (set-dispatch-macro-character #\# #\A 'scan-array *scan-table*)
  (set-dispatch-macro-character #\# #\| 'scan-big-comment *scan-table*)
  (make-dispatch-macro-character #\% t *scan-table*)
  (set-dispatch-macro-character #\% #\' 'scan-quantum-function *scan-table*))