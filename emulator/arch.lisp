;;;; Quantum emulator architecture
;;;; Author: Augustus Huang
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
