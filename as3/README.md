FaceIn
======
A prolog program answering questions about a social network graph.
by Jens Dahl Møllerhøj, (nkr889@alumni.ku.dk)

This is my hand in for the assignment found at [http://www.diku.dk/~kflarsen/ap-e2014/facein/facein.html]

The code can be compiled with `swipl`.
Open `swipl` and type `[people].` and hit enter to load the program.
Type `tests.` to run the test suite.

Notes
-----
- `chain` finds all solutions to the problem. That is, chain(G,X,Y) finds all
  the paths from X to Y.
- All the predicates has been documented in the code.
- I have tried to find the most compact solution.
- My tests uses the illigal \+ operator. They are not part of my solution.
- In my comments, $n denotes the n'th parameter (0 based).

Testing
-------
Writing tests in prolog turned out to be surpricingly easy. My simple test suite
covers all of the required functions with both a negative and positive test.

Room for improvement
--------------------
A better solution would not loop infinately. Eg. `chain_to_all` would stop after
the first path had been found.
I could have given my variables more descriptive names, however, I find that
single letters provides better readability.
