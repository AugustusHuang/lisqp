;;;; The MIT License (MIT)

;;;; Copyright (c) 2015 Huang Xuxing

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; asdf file

(in-package :asdf-user)

(defsystem :cl-lisqp
  :version "0.0.1"
  :licence "MIT"
  :description "A Common Lisp quantum library and quantum emulator."
  :components
  ((:cl-source-file "packages")
   (:module
    :lib
    :components
    ((:cl-source-file "util" :depends-on ("../packages"))
     (:cl-source-file "type" :depends-on ("../packages" "util"))
     (:cl-source-file "decohere" :depends-on ("../packages" "util" "type"))
     (:cl-source-file "gates"
		      :depends-on ("../packages" "type" "util" "decohere"))
     (:cl-source-file "error-correction"
		      :depends-on ("../packages" "type" "util" "decohere" "gates"))
     (:cl-source-file "qft"
		      :depends-on ("../packages" "type" "util" "gates"))
     (:cl-source-file "shor"
		      :depends-on ("../packages" "type" "util" "gates" "qft"))
     (:cl-source-file "grover"
		      :depends-on ("../packages" "type" "util" "gates"))
     (:cl-source-file "arithmetic"
		      :depends-on ("../packages" "type" "util" "gates"))))
   (:module
    :emulator
    :components
    ((:cl-source-file "error" :depends-on ("../packages"))
     (:cl-source-file "arithmetic"
		      :depends-on ("../packages" "../lib/arithmetic"))
     (:cl-source-file "qreg"
		      :depends-on ("../packages" "error" "arithmetic"))
     (:cl-source-file "opcode"
		      :depends-on ("../packages" "error" "arithmetic" "qreg"))))
   (:static-file "LICENSE")
   (:static-file "README.md")))
