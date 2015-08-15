# lisqp
A Common Lisp quantum library and quantum emulator.

## What's for?
Quantum computation is approaching!
But it's really awkward to define a quantum register with specific content and
then assign a meaning to it, instead I want to directly input a number,
generate a quantum register to represent it (or even call quantum
functions with classic arguments). Then we can have a canonical interface to
handle classical arithmetics and quantum arithmetics.

## How to build?
Current implement and test platform is SBCL. But since I only use ANSI or
standard functions, I guess it will run on other platforms.
In SBCL:

    CL-USER> (:require :cl-lisqp)

will load the quantum library and quantum emulator.
On platforms don't support `:require` function:

    CL-USER> (asdf:load-system :cl-lisqp)

Now it won't run... Still under development...
