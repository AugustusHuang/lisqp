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

;;;; Lisqp scanner
;;;; Date: May 27, 2015

;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defvar *read-base* 10 "The current input base.")
(defvar *read-eval* t "Controls the #. reader macro.")
(defvar *read-default-float-format* 'single-float)
(defvar *read-suppress* nil)
(declaim (special *readtable* *standard-readtable*)
	 (type readtable *readtable* *standard-readtable*))

(defstruct (readtable
	     (:predicate readtablep)
	     (:copier))
  (case :upcase :type (member :upcase :downcase :preserve :invert))
  (base-characters (make-array 256 :initial-element nil) :type (simple-vector 256))
  (extended-characters (make-hash-table) :type hash-table))

;;; TODO: At some point the init code must copy the standard readtable to create
;;; the initial readtable.
(defvar *protect-the-standard-readtable* nil)
(setf *standard-readtable* (make-readtable)
      *readtable* *standard-readtable*)

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  (when (eql to-readtable 'nil)
    (setf to-readtable (make-readtable)))
  (when (eql from-readtable 'nil)
    (setf from-readtable *standard-readtable*))
  (check-type to-readtable readtable)
  (check-type from-readtable readtable)
  (setf (readtable-case to-readtable) (readtable-case from-readtable))
  (dotimes (i 256)
    (set-syntax-from-char (code-char i) (code-char i) to-readtable from-readtable))
  (clrhash (readtable-extended-characters to-readtable))
  (maphash (lambda (k v)
	     (declare (ignore))
	     (set-syntax-from-char k k to-readtable from-readtable))
	   (readtable-extended-characters from-readtable))
  to-readtable)

(defun base-char-p (object)
  (and (characterp object)
       (= (char-bits object) 0)
       (< (char-code object) 256)))

(defun readtable-syntax-type (char &optional (readtable *readtable*))
  (check-type readtable (or readtable null) "a readtable designator")
  (check-type char character)
  (unless readtable
    (setf readtable *standard-readtable*))
  (cond ((base-char-p char)
	 ;; Base character.
	 (svref (readtable-base-characters readtable) (char-code char)))
	(t ;; Extended character.
	 (gethash char (readtable-extended-characters readtable) nil))))

(defun (setf readtable-syntax-type) (value char &optional (readtable *readtable*))
  (check-type readtable (or readtable null) "a readtable designator")
  (check-type char character)
  (unless readtable
    (setf readtable *standard-readtable*))
  (cond ((base-char-p char)
	 ;; Base character.
	 (setf (svref (readtable-base-characters readtable) (char-code char)) value))
	(t ;; Extended character.
	 (setf (gethash char (readtable-extended-characters readtable)) value))))

(defun get-macro-character (char &optional (readtable *readtable*))
  (let ((data (readtable-syntax-type char readtable)))
    (if (listp data)
	(values (first data) (second data))
	(values nil nil))))

(defun set-macro-character (char new-function &optional non-terminating-p (readtable *readtable*))
  (check-type new-function (or symbol function) "a function designator")
  (when (and (null readtable) *protect-the-standard-readtable*)
    (cerror "Carry on!" "This would modify the standard readtable."))
  (if new-function
      (setf (readtable-syntax-type char readtable)
	    (list new-function (not non-terminating-p)))
      (setf (readtable-syntax-type char readtable) nil))
  t)

(defun make-dispatch-macro-character (char &optional non-terminating-p (readtable *readtable*))
  (when (and (null readtable) *protect-the-standard-readtable*)
    (cerror "Carry on!" "This would modify the standard readtable."))
  (setf (readtable-syntax-type char readtable)
	(list 'read-dispatch-char (not non-terminating-p)
	      (make-array 256 :initial-element nil)
	      (make-hash-table)))
  t)

(defun get-dispatch-macro-character (disp-char sub-char &optional (readtable *readtable*))
  (check-type sub-char character)
  (let ((data (readtable-syntax-type disp-char readtable)))
    (unless (and (listp data) (= (length data) 4))
      (error "Character ~S is not a dispatching macro character." disp-char))
    (cond ((base-char-p sub-char)
	   (svref (third data) (char-code sub-char)))
	  (t (gethash sub-char (fourth data))))))

(defun set-dispatch-macro-character (disp-char sub-char new-function &optional (readtable *readtable*))
  (check-type sub-char character)
  (setf sub-char (char-upcase sub-char))
  (when (and (null readtable) *protect-the-standard-readtable*)
    (cerror "Carry on!" "This would modify the standard readtable."))
  (check-type new-function (or symbol function) "a function designator")
  (let ((data (readtable-syntax-type disp-char readtable)))
    (unless (and (listp data) (= (length data) 4))
      (error "Character ~S is not a dispatching macro character." disp-char))
    (cond ((base-char-p sub-char)
	   (setf (svref (third data) (char-code sub-char)) new-function))
	  (t (setf (gethash sub-char (fourth data)) new-function))))
  t)

(defun set-syntax-from-char (to-char from-char &optional (to-readtable *readtable*) from-readtable)
  (check-type to-char character)
  (check-type from-char character)
  (let ((data (readtable-syntax-type from-char from-readtable)))
    (cond ((not (consp data))
	   (setf (readtable-syntax-type to-char to-readtable) data))
	  ((= (length data) 4)
	   ;; Dispatching character, must copy the dispatch tables.
	   (let ((ht (make-hash-table)))
	     (maphash (lambda (k v) (setf (gethash k ht) v)) (fourth data))
	     (setf (readtable-syntax-type to-char to-readtable)
		   (list (first data) (second data)
			 (make-array 256 :initial-contents (third data))
			 ht))))
	  (t (setf (readtable-syntax-type to-char to-readtable)
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

;;; Set basic syntax traits for standard characters.
(setf (readtable-syntax-type #\Tab *standard-readtable*) :whitespace
      (readtable-syntax-type #\Newline *standard-readtable*) :whitespace
      (readtable-syntax-type #\Linefeed *standard-readtable*) :whitespace
      (readtable-syntax-type #\Page *standard-readtable*) :whitespace
      (readtable-syntax-type #\Return *standard-readtable*) :whitespace
      (readtable-syntax-type #\Space *standard-readtable*) :whitespace
      (readtable-syntax-type #\\ *standard-readtable*) :single-escape
      (readtable-syntax-type #\| *standard-readtable*) :multiple-escape)

;;; Function READ, READ-PRESERVING-WHITESPACE
;;; Function READ-DELIMITED-LIST

(defun follow-stream-designator (stream default)
  (cond ((null stream) default)
	((eql stream 't) *terminal-io*)
	((streamp stream) stream)
	(t (error 'type-error
		  :expected-type '(or stream null (eql t))
		  :datum stream))))

(defun whitespace[2]p (char &optional (readtable *readtable*))
  "Test if CHAR is a whitespace[2] character under READTABLE."
  (eql (readtable-syntax-type char readtable) :whitespace))

;;; TODO: Unicode awareness.
(defun invalidp (char &optional (readtable *readtable*))
  "Test if CHAR is an invalid character under READTABLE."
  (and (eql (readtable-syntax-type char readtable) nil)
       (or (member char '(#\Backspace #\Tab #\Newline #\Linefeed #\Page
			  #\Return #\Space #\Rubout)))))

(defun decimal-point-p (char)
  "Test if CHAR is a decimal point character."
  (eql char #\.))

(defun plus-sign-p (char)
  "Test if CHAR is a plus sign character."
  (eql char #\+))

(defun minus-sign-p (char)
  "Test if CHAR is a minus sign character."
  (eql char #\-))

(defun terminating-macro-p (char &optional (readtable *readtable*))
  (multiple-value-bind (fn terminatingp)
      (get-macro-character char readtable)
    (declare (ignore fn))
    terminatingp))

(defun read-token (stream first)
  "Read a normal Lisp token from STREAM with FIRST as the initial character."
  (when *read-suppress*
    ;; Read characters until EOF or a whitespace character or terminating macro character is seen.
    ;; When *READ-SUPPRESS* is in effect, a reduced version of the normal token reading
    ;; code is used. Package markers and dots are ignored and integers are not parsed.
    (do ((x first (read-char stream nil 'nil t)))
	((or (eql x 'nil)
	     (when (or (terminating-macro-p x) (whitespace[2]p x))
	       (unread-char x stream)
	       t)))
      (let ((syntax (readtable-syntax-type x)))
	(cond ((eql syntax :single-escape)
	       ;; Single escape character, read the next character directly.
	       (read-char stream t nil t))
	      ((eql syntax :multiple-escape)
	       ;; Multiple escape character, read characters until another multiple escape character
	       ;; is seen. Treat single escape characters as above.
	       (do ((y (read-char stream t nil t)
		       (read-char stream t nil t)))
		   ((multiple-escape-p y))
		 (when (single-escape-p y)
		   (read-char stream t nil t)))))))
    (return-from read-token nil))
  ;; Normal read code, without *READ-SUPPRESS* enabled
  (let ((token (make-array 16 :element-type 'character
			   :adjustable t
			   :fill-pointer 0))
	(seen-escape nil)
	(package-name nil)
	;; Set to true when a double package marker is used. ::
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
      ;; If an escape character was seen, then do not try to parse the token as
      ;; an number, intern it and return it immediately.
      (seen-escape
       (intern token))
      ;; Attempt to parse a number.
      (t (or (read-integer token)
             (read-float token)
             (read-ratio token)
             (intern token))))))

(defun read-ratio (string)
  ;; ratio = sign? digits+ slash digits+
  (let ((numerator nil)
        (denominator nil)
        (negativep nil)
        (start 0)
        (len (length string)))
    (when (zerop (length string))
      (return-from read-ratio))
    ;; Check for a leading sign.
    (case (char string 0)
      (#\- (incf start)
           (setf negativep t))
      (#\+ (incf start)))
    ;; Ensure at least one numerator digit.
    (when (>= start len)
      (return-from read-ratio))
    (setf numerator (digit-char-p (char string start) *read-base*))
    (when (not numerator)
      (return-from read-ratio))
    (loop
       (incf start)
       (when (>= start len)
         (return-from read-ratio))
       (let ((ch (char string start)))
         ;; Numerator is terminated by a slash.
         (when (eql ch #\/)
           (return))
         (let ((weight (digit-char-p ch *read-base*)))
           (when (not weight)
             (return-from read-ratio))
           (setf numerator (+ (* numerator *read-base*)
                              weight)))))
    ;; Skip over the slash.
    (incf start)
    ;; Ensure at least one denominator digit.
    (when (>= start len)
      (return-from read-ratio))
    (setf denominator (digit-char-p (char string start) *read-base*))
    (when (not denominator)
      (return-from read-ratio))
    (loop
       (incf start)
       (when (>= start len)
         (return))
       (let ((weight (digit-char-p (char string start) *read-base*)))
         (when (not weight)
           (return-from read-ratio))
         (setf denominator (+ (* denominator *read-base*)
                              weight))))
    (/ numerator denominator)))

(defvar *exponent-markers* "DdEeFfLlSs")
(defvar *decimal-digits* "0123456789")

(defun read-float (string)
  ;; float    = sign? decimal-digit* decimal-point decimal-digit+
  ;;          = sign? decimal-digit+ [decimal-point decimal-digit*] exponent
  ;; exponent = exponent-marker [sign] decimal-digit+
  ;; exponent-marker = d | D | e | E | f | F | l | L | s | S
  (let ((integer-part 0)
        (decimal-part 0.0)
        (saw-integer-digits nil)
        (saw-decimal-digits nil)
        (saw-decimal-point nil)
        (exponent #\F)
        (exponent-sign 1)
        (exponent-value 0)
        (sign 1)
        (position 0))
    (flet ((peek ()
             (when (< position (length string))
               (char string position)))
           (consume ()
             (prog1 (char string position)
               (incf position))))
      ;; Check for a leading sign.
      (case (peek)
        (#\- (consume)
             (setf sign -1))
        (#\+ (consume)))
      ;; Remaining string must not be empty.
      (when (null (peek))
        (return-from read-float))
      ;; Parse the integer portion.
      (loop
         (let ((weight (position (peek) *decimal-digits*)))
           (when (not weight) (return))
           (consume)
           (setf saw-integer-digits t)
           (setf integer-part (+ (* integer-part 10) weight))))
      ;; Parse the decimal portion.
      (when (decimal-point-p (peek))
        (setf saw-decimal-point t)
        (consume)
        ;; If there was an integer part, then the next character
        ;; must be either a decimal-digit or an exponent marker.
        ;; If there was no integer part, it must be a decimal-digit.
        (when (and (not (or (not saw-integer-digits)
                            (find (peek) *exponent-markers*)))
                   (not (find (peek) *decimal-digits*)))
          (return-from read-float))
        ;; Accumulate decimal digits.
        (let ((first-decimal position))
          (loop
             (when (not (find (peek) *decimal-digits*))
               (return))
             (setf saw-decimal-digits t)
             (consume))
          ;; Now works backwards and build the decimal part.
          (dotimes (i (- position first-decimal))
            (incf decimal-part (digit-char-p (char string (- position i 1))))
            (setf decimal-part (/ decimal-part 10)))))
      ;; And look for an exponent.
      (when (find (peek) *exponent-markers*)
        (setf exponent (consume))
        (case (peek)
          (#\- (consume)
               (setf exponent-sign -1))
          (#\+ (consume)))
        ;; Must be at least one digit in the exponent
        ;; and one digit in the integer part
        (when (or (not (find (peek) *decimal-digits*))
                  (not saw-integer-digits))
          (return-from read-float))
        ;; Read exponent part.
        (loop (when (not (find (peek) *decimal-digits*))
                (return))
           (setf exponent-value (+ (* exponent-value 10)
                                   (digit-char-p (consume))))))
      ;; Must be at the end.
      (when (peek)
        (return-from read-float))
      ;; TODO, deal with float type selection correctly.
      (* sign
         (+ integer-part decimal-part)
         ;; ### 10.0 to work around a missing feature in FLOAT. No bignum support.
         (expt 10.0 (* exponent-sign exponent-value))))))

(defun read-integer (token)
  "Attempt to parse TOKEN as an integer. Return false if it can't be parsed as an integer."
  ;; integer = sign? decimal-digit+ decimal-point
  ;;         = sign? digit+
  (let ((read-base *read-base*)
	(negative nil)
	(saw-sign nil)
	(number 0)
	(start 0)
	(end (length token)))
    ;; Check for a trailing decimal point, identifying a decimal integer
    (when (decimal-point-p (char token (1- (length token))))
      (setf read-base 10
	    end (1- end)))
    ;; Check for a leading sign.
    (when (or (plus-sign-p (char token 0))
	      (minus-sign-p (char token 0)))
      (setf saw-sign t
	    negative (minus-sign-p (char token 0))
	    start (1+ start)))
    ;; If the token is empty, aside from the sign, then it isn't a number.
    (when (not (= (- end start) 0))
      ;; Main digit-reading loop for integers.
      (do ((offset start (1+ offset)))
          ((>= offset end)
           ;; Successfully parsed an integer!
           (if negative
               (- number)
               number))
        (let ((weight (digit-char-p (char token offset) read-base)))
          (when (not weight)
            ;; This character is not a digit in the current base.
            (return))
          (setf number (+ (* number read-base) weight)))))))

(defun read-maybe-nothing (stream char)
  (let ((retval (multiple-value-list
                 (funcall (get-coerced-cmt-entry char *readtable*)
                          stream
                          char))))
    (when retval
      (setf (cdr retval) nil))))

;; Can't use read-delimited-list because it doesn't handle dotted lists
(defun read-left-parenthesis (stream first)
  "Read a possibly dotted list from STREAM."
  (declare (ignore first))
  (do* ((list (cons nil nil))
	(tail list))
       (nil)
    (let ((x (peek-char t stream t nil t)))
      (cond
	((eql x #\))
	 ;; Done, finish list
	 ;; Read the ) and drop it
	 (read-char stream t nil t)
	 (return (if *read-suppress* nil (cdr list))))
	((eql x #\.)
	 ;; Reading a potentially dotted list
	 ;; Read the dot and drop it
	 (read-char stream t nil t)
	 (let ((y (peek-char nil stream t nil t)))
	   (when (or (terminating-macro-p y) (whitespace[2]p y))
	     (let ((final (read stream t nil t)))
	       (when (eql list tail)
		 (error 'simple-reader-error :stream stream
			:format-control "No forms before dot in dotted list."
			:format-arguments '()))
	       (unless (char= (read-char stream t nil t) #\))
		 (error 'simple-reader-error :stream stream
			:format-control "Too many elements after dot in dotted list."
			:format-arguments '()))
	       (setf (cdr tail) final)
	       (return (if *read-suppress* nil (cdr list))))))
	 ;; Oops, it wasn't a dotted list and we can't unread
	 ;; the dot because of the peek-char call
	 ;; Manually dispatch to the next reader function
	 (let ((value (multiple-value-list
		       (funcall (or (get-macro-character x)
				    #'read-token)
				stream x))))
	   (when value
	     (setf (cdr tail) (cons (car value) nil))
	     (setf tail (cdr tail)))))
	(t (let* ((c (read-char stream t nil t))
		  (value (multiple-value-list (funcall (or (get-macro-character c)
							   #'read-token)
						       stream
						       c))))
	     (when value
	       (setf (cdr value) nil
		     (cdr tail) value
		     tail value))))))))

(defun read-right-parenthesis (stream first)
  "Signals a reader-error when an unexpected #\) is seen."
  (declare (ignore stream))
  (error 'simple-reader-error :stream stream
	 :format-control "Unexpected ~S."
	 :format-arguments (list first)))

(defun read-single-quote (stream first)
  "Reads a quoted form from STREAM."
  (declare (ignore first))
  (list 'quote (read stream t nil t)))

(defun read-semicolon (stream first)
  "Reads a newline-terminated comment from STREAM."
  (declare (ignore first))
  (do () ((char= (read-char stream nil #\Newline nil) #\Newline)))
  (values))

(defun read-double-quote (stream first)
  "Reads a string terminated by FIRST from STREAM."
  (do ((string (make-array 16
			   :element-type 'character
			   :adjustable t
			   :fill-pointer 0))
       (x (read-char stream t nil t)
	  (read-char stream t nil t)))
      ((eql x first) string)
    (if (eql (readtable-syntax-type x) :single-escape)
	(vector-push-extend (read-char stream t nil t) string)
	(vector-push-extend x string))))

(defun read-backquote (stream first)
  (declare (ignore first))
  (list 'backquote (read stream t nil t)))

(defun read-comma (stream first)
  (declare (ignore first))
  (case (peek-char nil stream t)
    (#\@ (read-char stream t nil t)
	 (list 'bq-comma-atsign (read stream t nil t)))
    (#\. (read-char stream t nil t)
	 (list 'bq-comma-dot (read stream t nil t)))
    (otherwise
     (list 'bq-comma (read stream t nil t)))))

(defun read-dispatch-char (stream first)
  "Dispatch to a dispatching macro character."
  (let ((c (read-char stream t nil t))
	(p nil))
    ;; If there's a leading number, read it
    (when (digit-char-p c)
      (setf p 0)
      (do () ((not (digit-char-p c)))
	(setf p (+ (digit-char-p c) (* p 10)))
	(setf c (read-char stream t nil t))))
    (setf c (char-upcase c))
    ;; Dispatch to the function
    (let ((fn (get-dispatch-macro-character first c)))
      (unless fn
	(error "No dispatch character defined for ~S." c))
      (funcall fn stream c p))))

(defun ignore-#-argument (ch p)
  "Issue a warning if p is not NIL."
  (when (and p (not *read-suppress*))
    (warn "Ignored numeric argument in #~A~A." p ch)))

(defun read-#-asterisk (stream ch p)
  (declare (ignore ch))
  (let ((bits (make-array 50 :element-type 'bit :adjustable t :fill-pointer 0)))
    (loop (let ((ch (read-char stream nil nil t)))
            (when (or (not ch) (whitespace[2]p ch) (terminating-macro-p ch))
              (when ch
                (unread-char ch stream))
              (return))
            (case ch
              (#\0 (vector-push-extend 0 bits))
              (#\1 (vector-push-extend 1 bits))
              (t (error "Invalid character ~S in #*." ch)))))
    (when (and p (not (zerop p)) (zerop (length bits)))
      (error "Need at least one bit for #* with an explicit length."))
    (when (and p (> (length bits) p))
      (error "Too many bits."))
    (let ((final-array (make-array (or p (length bits))
                                   :element-type 'bit)))
      (unless (zerop (length final-array))
        (fill final-array (aref bits (1- (length bits))))
        (setf (subseq final-array 0 (length bits)) bits))
      final-array)))

(defun read-#-backslash (stream ch p)
  (ignore-#-argument ch p)
  (let ((x (read-char stream t nil t))
	(y (peek-char nil stream nil nil t)))
    (if (or (eql nil y)
	    (get-macro-character y)
	    (whitespace[2]p y))
	;; Simple form: Single character followed by EOF or a non-constituent character.
	;; Just return the character that was read.
	x
	;; Reading a character name, similar to read-token, but no special handling
	;; is done for packages or numbers.
	(let ((token (make-array 1
				 :element-type 'character
				 :initial-element x
				 :adjustable t
				 :fill-pointer t)))
	  (do ((z (read-char stream nil nil t)
		  (read-char stream nil nil t)))
	      ((or (eql nil z)
		   (when (or (get-macro-character z) (whitespace[2]p z))
		     (unread-char z stream)
		     t)))
	    (let ((syntax (readtable-syntax-type x)))
	      (cond ((eql syntax :single-escape)
		     (vector-push-extend (read-char stream t nil t) token))
		    ((eql syntax :multiple-escape)
		     (do ((y (read-char stream t nil t)
			     (read-char stream t nil t)))
			 ((multiple-escape-p y))
		       (if (single-escape-p y)
			   (vector-push-extend (read-char stream t nil t) token)
			   (vector-push-extend y token))))
		    (t (vector-push-extend (case-correct z) token)))))
	  ;; Finished reading the token, convert it to a character
	  (let ((c (name-char token)))
	    (when (and (not c) (not *read-suppress*))
	      (error 'simple-reader-error :stream stream
                     :format-control "Unrecognized character name ~S."
                     :format-arguments (list token)))
	    c)))))

(defun read-#-quote (stream ch p)
  (ignore-#-argument ch p)
  (list 'function (read stream t nil t)))

(defun read-#-colon (stream ch p)
  (ignore-#-argument ch p)
  ;; Use read-token to read the symbol name as a keyword.
  ;; Reading it as a keyword suppresses the integer parsing code
  ;; and forces it to produce a symbol.
  ;; FIXME: This causes a symbol with the same name to be added
  ;; to the KEYWORD package as a side effect. It would be nice to
  ;; avoid that.
  (let ((token (read-token stream #\:)))
    (if *read-suppress*
	nil
	(make-symbol (symbol-name token)))))

(defun read-#-dot (stream ch p)
  (ignore-#-argument ch p)
  (cond (*read-suppress*
	 (read stream t nil t))
	(*read-eval*
	 (eval (read stream t nil t)))
	(t (error 'simple-reader-error :stream stream
                  :format-control "Cannot #. when *READ-EVAL* is false."
                  :format-arguments '()))))

(defun read-#-radix (stream ch p)
  "Read a number in the specified radix."
  ;; This function is used for #B, #O and #X in addition to #R
  (when *read-suppress*
    (read stream t nil t)
    (return-from read-#-radix))
  (let* ((bases '((#\B . 2) (#\O . 8) (#\X . 16)))
	 (fixed-base (assoc ch bases))
	 (base (cond (fixed-base
		      (when p
			(warn "Ignored explicit base in #~A~A" p ch))
		      (cdr fixed-base))
		     (p p)
		     (t (error "No radix specified in #~A" ch)))))
    (declare (type (integer 2 36) base))
    ;; Successfully computed what base to use, rebind
    ;; *READ-BASE* and read a value, checking that it's of type RATIONAL
    (let* ((*read-base* base))
      (the rational (read stream t nil t)))))

(defun read-#-left-parenthesis (stream ch p)
  (let ((foo (read-left-parenthesis stream #\()))
    (unless *read-suppress*
      (apply #'vector foo))))

(defun read-#-complex (stream ch p)
  "Read a complex number."
  (ignore-#-argument ch p)
  (when *read-suppress*
    (read stream t nil t)
    (return-from read-#-complex))
  (let ((number (read stream t nil t)))
    (when (or (not (listp number))
	      (/= (length number) 2)
	      (not (realp (car number)))
	      (not (realp (cadr number))))
      (error "Invalid complex number ~S" number))
    (complex (first number) (second number))))

(defun read-#-array (stream ch n-dimensions)
  (declare (ignore ch))
  (check-type n-dimensions array-axis)
  (when *read-suppress*
    (read stream t nil t)
    (return-from read-#-array))
  (let* ((object (read stream t nil t))
         (current-dim object)
         (dimensions (make-list n-dimensions :initial-element 0)))
    (dotimes (i n-dimensions)
      (let ((len (length current-dim)))
        (setf (elt dimensions i) len)
        (when (zerop len) (return))
        (setf current-dim (elt current-dim 0))))
    (make-array dimensions :initial-contents object)))

(defun read-#-pathname (stream ch p)
  (ignore-#-argument ch p)
  (cond (*read-suppress*
	 (read stream t nil t)
	 nil)
	(t (parse-namestring (read stream t nil t)))))

(defun eval-feature-test (test)
  "Evaluate the feature expression TEST."
  (etypecase test
    (symbol (member test *features*))
    (cons (case (car test)
	    (:not (when (or (null (cdr test)) (cddr test))
		    (error "Invalid feature expression ~S" test))
		  (not (eval-feature-test (cadr test))))
	    (:and (dolist (subexpr (cdr test) t)
		    (when (not (eval-feature-test subexpr))
		      (return nil))))
	    (:or (dolist (subexpr (cdr test) nil)
		   (when (eval-feature-test subexpr)
		     (return t))))
	    (t (error "Invalid feature expression ~S" test))))))

(defun read-#-features (stream suppress-if-false)
  "Common function to implement #+ and #-."
  (let* ((test (let ((*package* (find-package "KEYWORD")))
		 (read stream t nil t)))
	 (*read-suppress* (or *read-suppress*
			      (if suppress-if-false
				  (not (eval-feature-test test))
				  (eval-feature-test test))))
	 (value (read stream t nil t)))
    (if *read-suppress*
	(values)
	value)))

(defun read-#-plus (stream ch p)
  (ignore-#-argument ch p)
  (read-#-features stream t))

(defun read-#-minus (stream ch p)
  (ignore-#-argument ch p)
  (read-#-features stream nil))

(defun read-#-vertical-bar (stream ch p)
  "Read the nestable #| ... |# comment."
  (ignore-#-argument ch p)
  (loop
     (case (read-char stream t nil t)
       (#\#
        ;; Could be followed by | after an integer argument.
        (loop
           ;; Eat any decimal digits immediately after.
           (when (not (digit-char-p (peek-char nil stream t nil t) 10))
             (return))
           (read-char stream))
        ;; Look for | and recurse.
        (when (eql (peek-char nil stream t nil t) #\|)
          (read-char stream) ; eat |.
          (read-#-vertical-bar stream #\| nil)))
       (#\| ;; Check for |# and finish.
        (when (eql (peek-char nil stream t nil t) #\#)
          (read-char stream)
          (return)))))
  (values))

(defun read-#-invalid (stream ch p)
  "Handle explicitly invalid # dispatch characters."
  (declare (ignore stream p))
  (error 'simple-reader-error :stream stream
	 :format-control "Illegal syntax #~A."
	 :format-arguments (list ch)))

(defun read-common (stream eof-error-p eof-value recursive-p)
  ;; Skip leading whitespace.
  (loop (let ((c (read-char stream eof-error-p 'nil t)))
	  (when (eql c 'nil)
	    (return eof-value))
	  (when (invalidp c)
	    (error 'simple-reader-error :stream stream
		   :format-control "Read invalid character ~S."
		   :format-arguments (list c)))
	  (unless (whitespace[2]p c)
	    ;; Done reading whitespace. Dispatch to the appropriate
	    ;; read subfunction.
	    (let ((value (multiple-value-list (funcall (or (get-macro-character c)
							   #'read-token)
						       stream c))))
	      (when value
		(return (first value))))))))

(defun read (&optional input-stream (eof-error-p t) eof-value recursive-p)
  "READ parses the printed representation of an object from STREAM and builds such an object."
  (let ((stream (follow-stream-designator input-stream *standard-input*)))
    (with-stream-editor (stream recursive-p)
      (let ((result (read-common stream
                                 eof-error-p
                                 eof-value
                                 recursive-p)))
        (unless (or (eql result eof-value) recursive-p)
          ;; Munch trailing whitespace iff not at EOF and not in a recursive call.
          (let ((ch (read-char stream nil nil)))
            (when (and ch (not (whitespace[2]p ch)))
              (unread-char ch stream))))
        result))))

(defun read-preserving-whitespace (&optional input-stream (eof-error-p t) eof-value recursive-p)
  (read-common (follow-stream-designator input-stream *standard-input*)
	       eof-error-p
	       eof-value
	       recursive-p))

(defun read-from-string (string &optional (eof-error-p t) eof-value &key (start 0) end preserve-whitespace)
  (let (index)
    (values
     (with-input-from-string (stream string :start start :end end :index index)
       (if preserve-whitespace
           (read-preserving-whitespace stream eof-error-p eof-value)
           (read stream eof-error-p eof-value)))
     index)))

(defmacro with-standard-io-syntax (&body body)
  `(%with-standard-io-syntax (lambda () (progn ,@body))))

(defun %with-standard-io-syntax (fn)
  (let ((*package* (find-package-or-die "CL-USER"))
	(*print-array* t)
	(*print-base* 10)
	(*print-case* :upcase)
	(*print-circle* nil)
	(*print-escape* t)
	(*print-gensym* t)
	(*print-length* nil)
	(*print-level* nil)
	(*print-lines* nil)
	(*print-miser-width* nil)
	;;(*print-pprint-dispatch* standard-pprint-dispatch-table)
	(*print-pretty* nil)
	(*print-radix* nil)
	(*print-readably* t)
	(*print-right-margin* nil)
	(*read-base* 10)
	(*read-default-float-format* 'single-float)
	(*read-eval* t)
	(*read-suppress* nil)
	(*readtable* (copy-readtable nil)))
    (funcall fn)))

;;; Set standard reader macros.
(progn
  (set-macro-character #\( 'read-left-parenthesis nil *standard-readtable*)
  (set-macro-character #\) 'read-right-parenthesis nil *standard-readtable*)
  (set-macro-character #\' 'read-single-quote nil *standard-readtable*)
  (set-macro-character #\; 'read-semicolon nil *standard-readtable*)
  (set-macro-character #\" 'read-double-quote nil *standard-readtable*)
  (set-macro-character #\` 'read-backquote nil *standard-readtable*)
  (set-macro-character #\, 'read-comma nil *standard-readtable*)
  (make-dispatch-macro-character #\# t *standard-readtable*)
  (set-dispatch-macro-character #\# #\\ 'read-#-backslash *standard-readtable*)
  (set-dispatch-macro-character #\# #\' 'read-#-quote *standard-readtable*)
  (set-dispatch-macro-character #\# #\( 'read-#-left-parenthesis *standard-readtable*)
  (set-dispatch-macro-character #\# #\* 'read-#-asterisk *standard-readtable*)
  (set-dispatch-macro-character #\# #\: 'read-#-colon *standard-readtable*)
  (set-dispatch-macro-character #\# #\. 'read-#-dot *standard-readtable*)
  (set-dispatch-macro-character #\# #\B 'read-#-radix *standard-readtable*)
  (set-dispatch-macro-character #\# #\O 'read-#-radix *standard-readtable*)
  (set-dispatch-macro-character #\# #\X 'read-#-radix *standard-readtable*)
  (set-dispatch-macro-character #\# #\R 'read-#-radix *standard-readtable*)
  (set-dispatch-macro-character #\# #\C 'read-#-complex *standard-readtable*)
  (set-dispatch-macro-character #\# #\A 'read-#-array *standard-readtable*)
  (set-dispatch-macro-character #\# #\S 'read-#-struct *standard-readtable*)
  (set-dispatch-macro-character #\# #\P 'read-#-pathname *standard-readtable*)
  (set-dispatch-macro-character #\# #\= 'read-#-equal-sign *standard-readtable*)
  (set-dispatch-macro-character #\# #\# 'read-#-sharp-sign *standard-readtable*)
  (set-dispatch-macro-character #\# #\+ 'read-#-plus *standard-readtable*)
  (set-dispatch-macro-character #\# #\- 'read-#-minus *standard-readtable*)
  (set-dispatch-macro-character #\# #\| 'read-#-vertical-bar *standard-readtable*)
  (set-dispatch-macro-character #\# #\< 'read-#-invalid *standard-readtable*)
  (set-dispatch-macro-character #\# #\Newline 'read-#-invalid *standard-readtable*)
  (set-dispatch-macro-character #\# #\Space 'read-#-invalid *standard-readtable*)
  (set-dispatch-macro-character #\# #\Tab 'read-#-invalid *standard-readtable*)
  (set-dispatch-macro-character #\# #\Page 'read-#-invalid *standard-readtable*)
  (set-dispatch-macro-character #\# #\) 'read-#-invalid *standard-readtable*))

(in-package :cl-lisqp)

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

;;; Scanner table
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

(declaim (inline change-base))
(defun change-base (new-base)
  "Change the reading number base to 'new'."
  (setf *scan-base* new-base))

(declaim (inline base-char-p))
(defun base-char-p (char)
  (and (characterp char)
       (< (char-code char) 255)))

(declaim (inline point-p))
(defun point-p (char)
  (eql char #\.))

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

(declaim (inline invalid-p))
(defun invalid-p (char &optional (scan-table *scan-table*))
  "Test if 'char' is an invalid character in 'scan-table'."
  (and (eql (scan-table-syntax-type char scan-table) nil)
       (or (member char '(#\Backspace #\Tab #\Newline #\Linefeed #\Page
			  #\Return #\Space #\Rubout)))))

(declaim (inline terminating-macro-p))
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
	       (make-q-ratio (- (/ numerator denominator)))
	       (make-q-ratio (/ numerator denominator))))
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

(defun scan-float (token)
  "Scan a floating point number."
  (let ((int 0)
	(f 0.0)
	(int-pass nil)
	(float-pass nil)
	(float-point-pass nil)
	(expo #\F)
	(exp-value 0)
	;; Only multiply them to get the final result.
	(plus-p 1)
	(exp-plus-p 1)
	(pos 0)
	(digits "0123456789"))
    (flet ((peek ()
	     (when (< pos (length token))
	       (char token pos)))
	   (moveon ()
	     (prog1 (char token pos)
	       (incf pos))))
      (case (peek)
	(#\- (moveon)
	     (setf plus-p -1))
	(#\+ (moveon)))
      (when (null (peek))
	(return-from scan-float))
      (loop
	 (let ((weight (position (peek) digits)))
	   (when (not weight)
	     (return))
	   (moveon)
	   (setf int-pass t
		 int (+ weight (* int 10)))))
      (when (point-p (peek))
	(setf float-point-pass t)
	(moveon)
	(when (and (not (or (not int-pass)
			    (find (peek) "eEfF")))
		   (not (find (peek) digits)))
	  (return-from scan-float))
	(let ((first-decimal pos))
	  (loop
	     (when (not (find (peek) digits))
	       (return))
	     (setf int-pass t)
	     (moveon))
	  (dotimes (i (- pos first-decimal))
	    (incf f (digit-p (char token (1- pos))))
	    (setf f (/ f 10)))))
      ;; How about exponent?
      (when (find (peek) "eEfF")
	(setf expo (moveon))
	(case (peek)
	  (#\- (moveon)
	       (setf exp-plus-p -1))
	  (#\+ (moveon)))
	(when (or (not (find (peek) digits))
		  (not int-pass))
	  (return-from scan-float))
	(loop
	   (when (not (find (peek) digits))
	     (return))
	   (setf exp-value (+ (* exp-value 10)
			      (digit-p (moveon))))))
      ;; Then we are at the end.
      (when (peek)
	(return-from scan-float))
      (make-q-float (* sign (+ int f) (expt 10.0 (* exp-plus-p exp-value)))))))

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
	  (setf num (+ (* num scan-base) digit)))))
    (make-q-int num)))

(defun scan-token (stream first)
  "Scan a token from 'stream' with 'first' as the initial character."
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
      (t (or (scan-integer token)
	     (scan-ratio token)
	     (scan-float token)
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
  (let ((c (read-char stream t nil t))
	(i (peek-char nil stream nil nil t)))
    (if (or (eql i nil)
	    (get-scan-macro-character i)
	    (whitespace[2]p i))
	c
	(let ((token (make-array 1 :element-type 'character :initial-element c
				 :adjustable t :fill-pointer t)))
	  (do ((z (read-char stream nil nil t) (read-char stream nil nil t)))
	      ((or (eql z nil)
		   (when (or (get-scan-macro-character z)
			     (whitespace[2]p z))
		     (unread-char z stream)
		     t)))
	    (let ((syntax (scan-table-syntax-type c)))
	      (cond ((eql syntax :single-escape)
		     (vector-push-extend (read-char stream t nil t) token))
		    ((eql syntax :multiple-escape)
		     (do ((y (read-char stream t nil t) (read-char stream t nil t)))
			 ((multiple-escape-p y))
		       (if (single-escape-p y)
			   (vector-push-extend (read-char stream t nil t) token)
			   (vector-push-extend y token))))
		    (t
		     (vector-push-extend (case-correct z) token)))))
	  (let ((x (name-char token)))
	    (when (not x)
	      (error 'scanner-error
		     :stream stream
		     :format-control "Unrecognized character name ~S."
		     :format-arguments (list token)))
	    (make-q-char x))))))

(defun scan-string (stream char)
  "Scan a string terminated by 'char'."
  (declare (ignore ignore))
  (do ((c (read-char stream t nil t) (read-char stream t nil t))
       (str (make-array 16 :element-type 'character :adjustable t
			:fill-pointer 0)))
      ((eql c char) str)
    (if (eql (scan-table-syntax-type c) :single-escape)
	(vector-push-extend (read-char stream t nil t) str)
	(vector-push-extend c str))))

(defun scan-dispatch-char (stream first)
  "Scan a dispatching macro character."
  (let ((char (read-char stream t nil t)))
    ))

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
    (make-q-complex (first num) (second num))))

(defun scan-array (stream ignore c)
  "Scan an array."
  (declare (ignore ignore))
  (let* ((array (read stream t nil t))
	 (dim array)
	 (dims (make-list c :initial-element 0)))
    (dotimes (i c)
      (let ((len (length dim)))
	(setf (elt dims i) len)
	(when (zerop len)
	  (return))
	(setf dim (elt dim 0))))
    (make-q-array dims :initial-contents array)))

(defun scan-intern (stream eof-error-p eof-value recursive-p)
  (loop (let ((c (read-char stream eof-error-p 'nil t)))
	  (when (eql c 'nil)
	    (return eof-value))
	  (when (invalid-p c)
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
