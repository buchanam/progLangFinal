STACK-M  

Monica Holliday - hollidmo  
Mitchell Schenk - schenkmi  
Michaela Buchanan - buchanam  

A basic stack-based language which utilizes stack operations, similar to Forth, to perform arithmetic and logical operations. It includes features such as conditionals, loops, macros with more to come. The language has more than one stack to provide loop functionality and
uses a dictionary to provide the ability to define and call macros.

The language is designed to run from GHCI by loading the file stack.hs.

To use the stack-m language using ghci:  
    1) load the file stack.hs  
    2) type the command "stackm" followed by a list of valid commands  
        - eg "stackm [PushB (I 2), PushB (I 3), MOp Mul]" will produce the result Ok [I 6]
        - if the sequence of commands entered is not valid ([PushB (I 1), MOp Mul]) Error will be returned
  
A more complicated example is the implementation of Euclid's Algorithm to solve for the gcd of two numbers.  
    1) using ghci create the list of commands as follows:  
        - gcd = [SOp Over, SOp Over, COp Gt, IfElse [SOp Swap] [], While Gt (I 0) [SOp Dup, SOp Rot, MOp Mod], SOp Drop]
    2) use the stackm command to run the program in the language prepending the two numbers to get the gcd for at the beginning  
        - stackm ([PushB (I 210), PushB (I 45)] ++ gcd)  
    3) see the gcd returned as a stack with one element  
        - Ok [I 15]

Another example of a program that one could create using the stack-m language is summing up the numbers 0 to N.
In the example below the numbers 0-10 are summed.
    - stackm [PushB (I 0), PushB (I 0), While Lt (I 10) [PushB (I 1), MOp Add, SOp Dup, SOp Rot, MOp Add, SOp Swap], SOp Drop]
    - result after running is OK [I 55]

An example of showing the macro functionality makes a macro out of the sumExample, calls it and doubles it.
    - stackm [(Define "sum" sumExample), Call "sum", PushB (I 2), MOp Mul]
    - result after running is Ok [I 110]

These examples are already in the stack.hs file and can be run with the command in ghci "stackm gcdExample" or "stackm sumExample".

The acceptable commands are shown below
PushB (I <integer>) -- Pushes an integer onto the stack
PushB (S <string>) -- Pushes a string onto the stack
PushB (B <True/False>) -- Pushes a boolean onto the stack

SOp Drop - removes the top element from the stack
SOp Dup - duplicates the top element on the stack
SOp Swap - swaps the topmost and second topmost elements on the stack
SOp Over - takes the second topmost element on the stack and adds a copy to the top of the stack
SOp Rot - changes the order of the top three elements on the stack e.g. [a, b, c] -> [b, c, a]

BOp And - Logical and between two booleans
BOp Or - Logical or between two booleans
BOp Not - Logical negation of a boolean

MOp Add - Adds two integers
MOp Sub - Subtraction between two integers
MOp Mul - Multiplication of two integers
MOp Div - Division of two integers
MOp Mod - Modulus of two integers

COp Equ - Compares two values of the same type and places True on top of the stack if they are equal, False if they are not
COp Lt - Compares two integers determining if one is less than the other. Places boolean on top of stack.
COp Gt - Compares two integers determining if one is greater than the other. Places a boolean on the top of the stack.

StrOp Concat - Concatenates two strings.
StrOp (Slice Int Int) - Removes some of the characters from a string. eg Slice 0 2 of "HELLO" = "HEL"

IfElse Prog Prog - Conditional branch runs one of the programs depending on a boolean that is on the top of the stack.
Define Macro Prog - Defines a macro that can then be called later. E.g. Define "myFunc" [PushB (I 0)]
Call Macro - Calls a macro that has been defined
While CpCmd Block Prog - While loop where Block is the stopping case for the while loop and CpCmd is the comparision between the value on the top of the stack and the Block passed in.  The program prog will be run until the comparision between the value on the top of the stack with the passed in Block fails. CpCmd can be Equ, Gt, Lt, and Block can be (I <integer>), (S <string>), or (B <boolean>).
