import Msm

-- POP, PUSH
test1 = runMSM [PUSH 1,PUSH 2, PUSH 3,POP,POP,HALT] == Right 1

-- DUP
test8 = runMSM [PUSH 1,DUP,POP,HALT] == Right 1

-- NEG
test9 = runMSM [PUSH 1,NEG,HALT] == Right (-1)

-- SWAP
test10 = runMSM [PUSH 1,PUSH 2,SWAP,POP,HALT] == Right 2

-- ADD
test11 = runMSM [PUSH 1,PUSH 2,ADD,HALT] == Right 3

-- JMP
test12 = runMSM [PUSH 3,JMP,HALT,PUSH 1,HALT] == Right 1

-- CJMP
test13 = runMSM [PUSH (-1),CJMP 4,PUSH 3,HALT,PUSH 1,HALT] == Right 1
test14 = runMSM [PUSH 0,CJMP 4,PUSH 3,HALT,PUSH 1,HALT]  == Right 3

-----------------
-- ERROR HANDLING
-----------------

-- POP, DUP, LOAD, NEG, JMP, CJMP, or HALT is executed with an empty stack.
test2 = runMSM [POP,HALT] == Left (Error {t = StackUnderflow})
test3 = runMSM [DUP,HALT] == Left (Error {t = StackUnderflow})
test4 = runMSM [NEG,HALT] == Left (Error {t = StackUnderflow})
test5 = runMSM [JMP,HALT] == Left (Error {t = StackUnderflow})
test6 = runMSM [CJMP 1,HALT] == Left (Error {t = StackUnderflow})
test7 = runMSM [HALT] == Left (Error {t = StackUnderflow})

-- STORE, SWAP, or ADD is executed on a stack containing less than two elements.
test17 = runMSM [PUSH 1,SWAP] == Left (Error {t = StackUnderflow})
test18 = runMSM [PUSH 1,ADD] == Left (Error {t = StackUnderflow})

-- LOAD or STORE is used on a register that has not been allocated with NEWREG.

-- NEWREG is called with an already allocated register.

-- The PC does not point to an instruction. That is, it is greater than or equal to the size of the program, or negative.
test15 = runMSM [PUSH (-1),CJMP 3,HALT]  == Left (Error {t = InvalidPC})
test16 = runMSM [PUSH (-1),CJMP (-3),HALT]  == Left (Error {t = InvalidPC})

main :: IO()
main = do
  putStrLn $ show test1
  putStrLn $ show test2
  putStrLn $ show test3
  putStrLn $ show test4
  putStrLn $ show test5
  putStrLn $ show test6
  putStrLn $ show test7
  putStrLn $ show test8
  putStrLn $ show test9
  putStrLn $ show test10
  putStrLn $ show test11
  putStrLn $ show test12
  putStrLn $ show test13
  putStrLn $ show test14
  putStrLn $ show test15
  putStrLn $ show test16
  putStrLn $ show test17
  putStrLn $ show test17
  putStrLn $ show test18
