# hcalculator

## BNF grammar

```
term -> [ term ( "+" | "-" ) ]* factor;
factor -> [ factor ( "*" | "/" ) ]* unary;
unary -> ("-") unary | primary;
primary -> NUMBER;
```
