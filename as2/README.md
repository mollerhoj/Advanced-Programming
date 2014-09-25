CurvySyntax
=========================
A parser for parsing a language that define curves.
by Jens Dahl Møllerhøj, (nkr889@alumni.ku.dk)

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

Testing
-------
I tried HUnit for testing. I do not like the framework very much.

Room for improvement
--------------------
I would also have liked to test extensively (eg. with QuickTest), but I could not find the time.
