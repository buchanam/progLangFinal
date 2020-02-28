Team Members:  
Monica Holliday - hollidmo  
Mitchell Schenk - schenkmi  
Michaela Buchanan - buchanam  

The language we developed is called stack-m which is a stack based language.  
The language implements all the basic data types including integers, booleans, and strings.  Additionally the language can be used to define and call macros, loop over a sequence of commands, and perform conditional logic.  

To use the stack-m language using ghci:
    1) load the file stack.hs
    2) type the command "stackm" followed by a list of valid commands
        - eg "stackm [PushB (I 2), PushB (I 3), Mul]" will produce the stack [I 6]

A more complicated example is the implementation of Euclid's Algorithm to solve for the gcd of two numbers.
    1) using ghci create the list of commands as follows:
        - gcd = [SOp Over, SOp Over, Gt, IfElse [SOp Swap] [], PushB (I 0), PushB (P [SOp Dup, SOp Rot, Mod]), While Gt, SOp Drop]
    2) use the stackm command to run the program in the language prepending the two numbers to get the gcd for at the beginning
        - stackm ([PushB (I 210), PushB (I 45)] ++ gcd)
    3) see the gcd returned as a stack with one element
        - [I 15]
