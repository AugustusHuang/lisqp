(in-package :cl-quantum-emulator)

(define-condition emulator-error (error) ())

(define-condition opcode-error (emulator-error) ()
  (:report "Invalid opcode."))