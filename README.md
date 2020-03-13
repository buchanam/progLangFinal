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
