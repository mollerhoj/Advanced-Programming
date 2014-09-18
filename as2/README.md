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
- I use a mix of `<++` and `<|>`+`notFollowedBy` to avoid ambiguous grammars. I chose to do so, because haven't tried either, and wanted to make a decision after trying both. I think I favor `<++`.

- My error ...

- I use the chaining of terms...

Testing
-------
I tried HUnit for testing. I do not like the framework very much.
Room for improvement
--------------------
I would also have liked to test extensively (eg. with QuickTest), but I could not find the time.
