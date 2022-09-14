# hcalculator - simple calculator written in Haskell

This is a simplistic expression calculator using a monadic parser with Parsec
as a small learning project.

## Installation and running

To run the program using stack:

```
stack run -- '3 * (4 + 5)'
27
```

Starting the program without arguments starts a Repl:

```
stack run
hcalc> 3 * (4 + 5)
27
hcalc> 503 * 333
16599
hcalc> 2 / 0
Division by zero not allowed.
hcalc> 34 +
Error: "hcalc" (line 1, column 5):
unexpected end of input
expecting white space, "(", "+", "-" or a number
hcalc> 1 + 1 + 3.3
5.3
```

Alternatively, build an executable with `stack build`.

## BNF grammar

The BNF grammar of the calculator is as follows:

```
expression -> term
term -> [ term ( "+" | "-" ) ]* factor;
factor -> [ factor ( "*" | "/" ) ]* power;
power -> [ power "^" ]* unary;
unary -> ("-") unary | primary;
primary -> NUMBER | "(" term ")";

NUMBER -> DIGIT+ ("." DIGIT+)?
```
