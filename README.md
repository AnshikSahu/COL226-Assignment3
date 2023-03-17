## COL226-Assignment3
# Calculator
##### Submitted by: Anshik Sahu

## Instructions to use:
#### How to start-

1. Open terminal.
2. Go to the directory containing all files.
3. Run "sudo sml".
4. Type your password and press enter key.
5. Run "CM.make "makefile.cm";".
6. Run "Calc.parse();".

#### How to use-

1. Type your expression and press enter.
2. Your expression can include:
    - Binary operators: "+", "-", "*" and "/" 
    - Negation: "~" (Warning: Not a operator, can only be used on values)
    - Integers: Use "~" instead of "-" to denote negative numbers.
    - Decimals: In the fractional-normal form.(Use "~" instead of "-" to denote negative numbers)

## Grammar for representing Rationals

Rational       ::= "~" rational | rational <br>
rational       ::= integer | decimal | fraction <br>
integer        ::= digit | digitinteger <br>
decimal        ::= integer "." integer "(" integer ")" | "." integer "(" integer ")" | integer "." "(" integer ")" <br>
fraction       ::= integer "/" nonzerointeger <br>
nonzerointeger ::= integer positivedigits <br>
positivedigits ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" <br>
digit         ::= positivedigits | "0" <br>

## Grammar used for representing values in parser

NUM    ::= integer | decimal <br>
integer::= digit | digit integer <br>
decimal::= integer "." integer "(" integer ")" | "." integer "(" integer ")" | integer "." "(" integer ")" <br>
digit  ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" <br>

## Grammar for arithmetic

Expression ::= Term | Term "+" Term | Term "-" Term <br>
Term       ::= Rat | Rat "*" Rat | Rat "/" Rat <br>
Rat        ::= RAT | variable <br>
RAT        ::= Rational | "(" Expression ")" <br>
variable   ::= alphabet | alphabet variable <br>
alphabet   ::= “A” | “B” | “C” | “D” | “E” | “F” | “G” | “H” | “I” | “J” | “K” | “L” | “M” | “N” | “O” | <br>
                 “P” | “Q” | “R” | “S” | “T” | “U” | “V” | “W” | “X” | “Y” | “Z” | “a” | “b” | “c” | “d” | <br>
                 “e” | “f” | “g” | “h” | “i” | “j” | “k” | “l” | “m” | “n” | “o” | “p” | “q” | “r” | “s” | <br>
                 “t” | “u” | “v” | “w” | “x” | “y” | “z” <br>

## Grammar used for representing expressions in parser

EXP        ::= NUM | EXP "+" EXP | EXP "-" EXP | EXP "*" EXP | EXP "/" EXP | "(" EXP ")" <br>
(The rules for associativity and precedence are mentioned separately in the parser) <br>

### Acknowledgements

I have used the resources present in the SMLNJ Documentation. Namely the base structure for the parser as present in the Rational_parser.lex, Rational_parser.grm, Rational_parser.sml files is taken from the above mentioned source.