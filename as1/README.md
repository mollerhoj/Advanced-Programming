Micro Stack Machine (Msm)
=========================
A virtual machine, able to run minor programs. 
by Jens Dahl Møllerhøj, (nkr889@alumni.ku.dk)

This is my hand in for the assignment found at [http://www.diku.dk/~kflarsen/ap-2014/notes/msm.html].

The code can be compiled with `ghc`.
Inspecting the code with `ghc -Wall -fno-warn-unused-do-bind` returns no warnings.
Inspecting the code with `hlint` returns no 2 warnings, both of which I disagree with.

Notes
-----
- The program counter is implemented as an integer
- I would have liked to only export runMSM, Error and Prog. But I could not figure out how. Instead, I export their internal representation.

- My registers have a(n unusual property): They can contain either nothing or (just) an Int, or they can of course be unallocated. If we try to load from an allocated register, where nothing is stored, we get an "Unspec "register empty" error. See test 24 for an example

- The MSM monad takes a state and returns either A) a value and a new state or B) an Error.

Testing
-------
A (very) simple test suite is found in MsmSpec.hs. Running the tests prints out 24 times 'True'. It is shown that all the instuctions works for my simple examples. It is also shown that all errorTypes can be triggered. 

Room for improvement
--------------------
I mentioned in the code-comments. I would also have liked to test more extensively (eg. with QuickTest, but I could not find the time.
