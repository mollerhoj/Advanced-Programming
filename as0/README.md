Curves
======
A library for working with piecewise linear curves in the plane.
by Jens Dahl Møllerhøj, (nkr889@alumni.ku.dk)

This is my hand in for the assignment found at [http://www.diku.dk/~kflarsen/ap-2014/curves/curves.html].

The code can be compiled with `ghc`. Inspecting the code with `hlint` nor `ghc -Wall` returns no warnings.

A had to change the implementation of `reflect` in order to draw correct hilbert curves. The tests `test6` and `test61` shows how my implementation diffes from the required. (Horizontal and Vertical) reflection is reversed.

Types
-----
Points are implemented as a pair of doubles.
Curves are implemented as a list of points.

Util funcitons
--------------
A list of helper functions I implemented and used:
- `rev` takes a vector in 2d space, and returns its opposite.
- `sequments` takes a curve and returns a list of the line sequments of the curve.
- `move` takes a point and a vector, and moves the point along the vector.
- `rotateRad` takes a radian value and rotates the curve around the origin.
- `rotateRadPoint` takes a radian value and rotate the point around the origin.
- `strangeRound` takes a double and rounds it as required by the assignment. It is strange, because
0.009 == 0.00 instead of the usual 0.009 == 0.01.

Render functions
----------------
A list of functions I implemented and used for rendering the curves in svg format:
`toSvg` renders a curve in svg format.
`svgHead` renders the svg header for a curve.
`svgBody` renders the svg body for a curve.
`svgLine` renders a svg line for for a curve seqment.
`svgTail` renders the svg tail for a curve.
`svgDouble` takes a double and renders it for the svg format.

Questions
---------
A often end up with a function that prepares for a (sometimes recursive) util function. see width', height' and bbox' for an example. I don't know how to name these util functions, and I don't know if it is an anti pattern to use them.

Testing
-------
A (very) simple test suite is found in CurvesSpec.hs. Running the tests prints out 11 times 'True', and creates or overwrites the file "test.html". "test.html" contains a svg image of a hilbert curve. The test suite is a simple indicator that the functions behave as expected. All public functions have one test.
