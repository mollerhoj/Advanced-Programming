CurvySyntax
=========================
A parser for parsing a language that define curves.
by Jens Dahl Møllerhø

This is my hand in for the assignment found at [http://www.diku.dk/~kflarsen/ap-e2014/curves/curves-syntax.html]

The code can be compiled with `ghc`.
Inspecting the code with `ghc -Wall -fno-warn-unused-do-bind` returns no warnings.
Inspecting the code with `hlint` returns no 1 warning, that I don't believe should be adressed.

Notes
-----
- I have defined a `finally` function that makes sure the parser have reached the end of the input. But in order to return a somewhat descriptive error, I don't use it.

- I defined a `more` function, that works like munch, but without the predicate.

- The `token` is used in the outer most parser (eg. it is used as little as possible.)

- Parsers return all possible results, only the result that reaches the eof is used.

I chose SimpleParse because the source code was easy to read. If I need more
features than SimpleParse can provide, I would like to implement them myself,
so that I understand the underlying implementation of my framework completely.
This way, I grok the implementation, and I don't have to memorize an API. 
Whenever I need a certain kind of functionallity, I look for it in the source.

In order to preserved the usual mathematical precedence of algebraic
calculations, the following non-terminals were defined:
- factors: numbers or parenthesed expressions
- terms: a list of factors multiplied or divided with each other.
- expressions: a list of terms added or subtracted from each each other.

I removed left recursion in the following way.
In the specified grammar, the non-terminal 'Curve' is left recursive. The grammar
was redefined in the following way to overcome this issue:

A nonterminal curveR was defined as a either a single point, an identifier or
a parenthesed curve. The all curves were defined as a starting with a curveR.

The grammar is no longer left recursive:
```
Program ::= Defs
Defs  ::= Def
       |  Def Defs
Def   ::= Ident '=' Curve
       |  Ident '=' Curve 'where' '{' Defs '}'
Curve ::= CurveR '++' Curve
       |  CurveR '^' Curve
       |  CurveR '->' Point
       |  CurveR '**' Expr
       |  CurveR 'refv' Expr
       |  CurveR 'refh' Expr
       |  CurveR 'rot' Expr
CurveR::= '(' Curve ')'
       |  Point
       |  Ident
Point ::= '(' Expr ',' Expr ')'
Expr  ::= Expr '+' Expr
       |  Expr '*' Expr
       |  'width' Curve
       |  'height' Curve
       |  Number
       |  '(' Expr ')'
Term ::= Factor '*' Factor
       | Factor '/' Factor
Factor:= Number
				 '(' Number ')'
Number:= Digits '.' Digits
			 | Digits
Digits:= Digit
				 Digits
Digit := 0 | 1 | 2 ...
```

Testing
-------
I tried HUnit for testing. I do not like the framework very much. I would have
liked to test extensively (eg. with QuickTest), but I could not find the time.

My tests shows that
1. A curve can be defined as a translated point
2. A curve can be defined to be single point
3. A curve can be a single point rotated around the origin
4. Curves can be defined as identifers, and more than one curve can be defined.
5. A curve can be a list of points
6. A program cannot be an empty string
7. Identifers cannot contain spaces
8. Points are two number separated by commas
9. A curve cannot be connected to an expression.

Room for improvement
--------------------
I would have liked to structure the code more, and use the parsers in a more
consistent manner. (Use the eof parser instead of notFollowedBy, and use
notFollowedBy instead of `if ... then reject`. I would also have liked to write
an order of magitude more unit tests. Especially for checking for whitespace.
