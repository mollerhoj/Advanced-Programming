-- Notes
-- The program counter is implemented as an integer
module Msm
( Stack (..)
, Regs (..)
, Inst (..)
, PC (..)
, Prog (..)
, State (..)
, ErrorType (..)
, Error (..)
, runMSM
) where

import Control.Monad

-- TODO: what is this?
import qualified Data.Map as Map

type Stack = [Int] 
type Regs = [Int]
data Inst = PUSH Int
          | POP 
          | HALT 
          | DUP
          | SWAP
          | NEG
          | ADD
          | NEWREG Int
          | LOAD
          | STORE
          | JMP
          | CJMP Int
            deriving (Show)
type PC = Int

-- Prog represents programs (lists of instructions)
type Prog = [Inst]

data State = State {
              prog  :: Prog,
              pc    :: PC,
              stack :: Stack,
              reg   :: Regs
             }
            deriving (Show)

data ErrorType = StackUnderflow | UnallocatedRegister Int | RegisterAlreadyAllocated | InvalidPC | Unspec String deriving (Show,Eq)

data Error = Error { t :: ErrorType } deriving (Show,Eq)

initial :: Prog -> State
initial p = State {
              prog = p,
              pc = 0,
              stack = [],
              reg = []
            }

-- The MSM monad takes a state and returns either A) a value and a new state or 
-- B) an Error.
newtype MSM a = MsM (State -> Either Error (a,State))

instance Monad MSM where
  --return x :: State -> (x,State)
  return x = MsM (\s -> Right (x,s))
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- see notes for picture.
  --
  -- (>>=) :: (State -> (a,State)) -> (a -> (State -> (b,State))) -> (State -> (b,State))
  --                    h                           f                     result                  
  -- I have a feeling that there is a nicer way to combine two monads
  -- I would like to not have to reimplement the Either monad here.
  (MsM h) >>= f = MsM $ \s -> case h s of 
                                Left x -> Left x
                                Right (a,s') -> let (MsM g) = f a
                                                              in g s'

inc :: MSM Int
inc = do s <- get
         set (s {pc = pc s + 1})
         return (pc s)

die :: ErrorType -> MSM a
die tx = MsM (\s -> Left Error {t = tx})

push :: Int -> MSM ()
push x = do s <- get
            set (s {stack = x : (stack s)})

pop :: MSM Int
pop = do s <- get
         e <- emptyStack
         if not e
         then do set (s {stack = tail (stack s)})
                 return (head (stack s))
         else do die StackUnderflow

dup :: MSM Int
dup = do x <- pop
         push x
         push x
         return x

get :: MSM State
get = MsM (\s -> Right (s,s))

set :: State -> MSM ()
set m = MsM (\s -> Right ((),m))

add :: MSM Int
add = do x <- pop
         y <- pop
         let z = (x+y)
         push z
         return z

swap :: MSM ()
swap = do x <- pop
          y <- pop
          push x
          push y

-- required by exercise? But I like get/set more...
modify :: (State -> State) -> MSM ()
modify f = do s <- get
              set (f s)
              return ()

neg :: MSM Int
neg = do x <- pop
         push (-x)
         return (-x)

getInst :: MSM Inst
getInst = do s <- get
             if inRange (pc s) (prog s)
             then do return (prog s !! pc s)
             else do die InvalidPC

-- Is the index inside the list?
inRange :: Int -> [a] -> Bool
inRange i l = i >= 0 && i < length l

emptyStack :: MSM Bool
emptyStack = do s <- get
                if (stack s) == []
                then do return True
                else do return False

interpInst :: Inst -> MSM Bool
interpInst POP      = do pop
                         inc
                         return True

interpInst (PUSH x) = do push x
                         inc
                         return True
interpInst DUP      = do dup
                         inc
                         return True
interpInst NEG      = do neg
                         inc
                         return True
interpInst HALT     = do return False

interpInst ADD      = do add
                         inc
                         return True

interpInst JMP      = do x <- pop
                         s <- get
                         set (s {pc = x})
                         return True

interpInst (CJMP i) = do x <- pop
                         s <- get
                         if x < 0
                         then do set (s {pc = i})
                                 return True
                         else do inc
                                 return True

interpInst SWAP      = do x <- pop
                          y <- pop
                          push x
                          push y
                          inc
                          return True

interp :: MSM ()
interp = run
  where run = do inst <- getInst
                 cont <- interpInst inst
                 when cont run

-- HALT error is handled in an ugly way here..
runMSM :: Prog -> Either Error Int
runMSM p = case runMSMHelper p of
                            Left x       -> Left x
                            Right ((),s) -> case stack s of
                                             []      -> Left (Error {t = StackUnderflow})
                                             (x:xs)  -> Right x

runMSMHelper :: Prog -> Either Error ((), State)
runMSMHelper p = let (MsM f) = interp 
           in f $ initial p

a = [PUSH 2,POP,PUSH 7,HALT]
