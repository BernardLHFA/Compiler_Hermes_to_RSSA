This is a short introduction for how to use the RSSA interpreter 
and compiler.

To compile and run the programs, you need to have Moscow ML installed,
including mosmlex and mosmlyac (https://mosml.org).  

You can compile the interpreter by

source compile.sh

This will generate a hi executable.

To run it, write

./hi program

or

./hi -r program

where program.cl is a RSSA program.  Note that you should not write
the extension.  Using the -r option will run the program backwards.

The interpreter will read input for the memory, run the first 
procedure in the program file, show the resulting program in RSSA,
read input from standard input and write output to standard output, 
as well as the resulting memories. Each parameter to be proceduced 
is entered and written on a single line. Execution starts when all 
inputs have been entered.  Output is one parameter per line as above,
but all numbers are written in hexadecimal format.

Example:

./hi example
Initialize Secret Memory: (*The memories are initialised as empty*)
Initialize Public Memory:
5

outputs

0x5
0x8 

You can compile the compiler by

source compileFull.sh

This will generate a main executable.

To run it, write

./main program

or

./main -r program

where program.hms is a Hermes program.  Note that you should not write
the extension.  Using the -r option will run the program backwards.

As for the interpreter, the compiler will read input for the memory, 
run the first procedure in the program file, print the resulting program
in RSSA, read input from standard input and write output to standard output, 
as well as the resulting memories. Each parameter to be proceduced 
is entered and written on a single line. Execution starts when all 
inputs have been entered.  Output is one parameter per line as above,
but all numbers are written in hexadecimal format.

Example:

./main Hermes/speck128
Initialize Secret Memory: 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0 34 0 0 0 0 0 0 0 56
0 0 0 0 0 0 0 78
Initialize Public Memory:
0
2
16
2

outputs

Secret Array: [0x6e, 0x3b, 0x8b, 0x38, 0xf8, 0xcb, 0xe5, 0x53, 0x78, 0x1a, 0x79, 
0x9e, 0xdd, 0xc7, 0x17, 0x72, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x38, 0x0, 0x0, 
0x0, 0x0, 0x0, 0x0, 0x0, 0x4e, 0x0, 0x0, ...]
0x0
0x2
0x10
0x2

It is important to notice that the input given is different from the Hermes
Interpreter, which only takes the values in the arrays as input. For more 
details on the Hermes Interpreter, please see the ReadMe in the Hermes folder. 
