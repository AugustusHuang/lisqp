# Files, directories...
emulator/ will contain all codes behave the quantum way, the opcodes if we
disassemble our high-level codes, and the elementary logical units made up
with quantum gates.  
lib/ will contain all codes run in the classic context, some utilities to
handle and take snapshots of our quantum registers, since it's only an
approximation, it will work without measurement, but we can't grab more
information than classical ones.  
lisqp/ will contain all codes about the compiler-interpreter, it can directly
run the codes in lisqp, or disassemble them into a quantum opcode form.
