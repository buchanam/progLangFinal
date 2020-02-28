STACK-M  

Monica Holliday - hollidmo  
Mitchell Schenk - schenkmi  
Michaela Buchanan - buchanam  

A basic stack-based language which utilizes stack operations, similar to Forth, to perform arithmetic and logical operations. It includes features such as conditionals, loops, macros with more to come. The language has more than one stack to provide loop functionality and
uses a dictionary to provide the ability to define and call macros.

The language is designed to run from GHCI loading stack.hs.

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
