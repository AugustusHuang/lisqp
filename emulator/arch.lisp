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

;;;; Quantum emulator architecture
;;;; Date: June 5, 2015

(in-package :cl-quantum-emulator)

(defvar *opcode-counter* 0)
(defvar *current-q-registers* nil)

(defun opcode-forward (step)
  "Step forward in the opcode array."
  (declare (type fixnum step))
  (incf *opcode-counter* step))

(defun opcode-backward (step)
  "Step backward in the opcode array."
  (declare (type fixnum step))
  (decf *opcode-counter* step))

(defun add-q-register (qreg)
  "Add a new quantum register to current list."
  (declare (type quantum-register qreg))
  (setf *current-q-registers* (append *current-q-registers* (list qreg))))

(defun remove-q-register (n)
  "Remove a quantum register from the current list."
  (declare (type fixnum n))
  (setf *current-q-registers* (remove (nth n *current-q-registers*)
				      *current-q-registers*)))
