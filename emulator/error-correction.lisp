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

;;;; Error correction code
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

(defun ec-encode (width qreg)
  "Encode a quantum-register, support error correction."
  (declare (type fixnum width)
	   (type quantum-register qreg))
  (let ((current-level (get-decoherence-level))
	(full-width (get-q-width qreg)))
    (set-decoherence-level 0)
    (loop for i from 0 to (- full-width 1) do
	 (if (= i (- full-width 1))
	     (set-decoherence-level current-level))
	 (if (< i width)
	     (progn
	       (hadamard (+ i full-width) qreg)
	       (hadamard (* 2 (+ i full-width)) qreg)
	       (c-not (+ i full-width) i qreg)
	       (c-not (* 2 (+ i full-width)) i qreg))
	     (progn
	       (c-not i (+ i full-width) qreg)
	       (c-not i (* 2 (+ i full-width)) qreg))))
    (set-ec-width full-width)
    (setf full-width (* 3 full-width))))

(defun ec-decode (width qreg)
  "Decode a quantum-register with error correction."
  (declare (type integer width)
	   (type quantum-register qreg))
  (let* ((current-level (get-decoherence-level))
	 (full-width (get-q-width qreg))
	 (decode-width (/ full-width 3)))
    (set-decoherence-level 0)
    (set-ec-width 0)
    (loop for i from (- decode-width 1) downto 0 do
	 (if (= i 0)
	     (set-decoherence-level current-level))
	 (if (< i width)
	     (progn
	       (c-not (+ i (* 2 decode-width)) i qreg)
	       (c-not (+ i decode-width) i qreg)
	       (hadamard (+ i (* 2 decode-width)) qreg)
	       (hadamard (+ i decode-width) qreg))
	     (progn
	       (c-not i (+ i (* 2 decode-width)) qreg)
	       (c-not i (+ i decode-width) qreg))))
    (loop for i from 1 to decode-width do
	 (let ((a (measure decode-width qreg))
	       (b (measure (- (* 2 (decode-width)) i) qreg)))
	   (if (and (= a 1)
		    (= b 1)
		    (< (- i 1) width))
	       (pauli-z (- i 1) qreg))))))
