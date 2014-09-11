-- Notes
-- The program counter is implemented as an integer
module MSM where

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

data ErrorType = StackUnderflow | UnallocatedRegister Int | RegisterAlreadyAllocated | InvalidPC | Unspec String

data Error = Error { errorType :: ErrorType, message :: String }

initial :: Prog -> State
initial p = State {
              prog = p,
              pc = 0,
              stack = [],
              reg = []
            }

-- get :: MSM State

--MSM Monad
newtype MSM a = MsM (State -> (a,State))

instance Monad MSM where
  --return x :: State -> (x,State)
  return x = MsM (\s -> (x,s))
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- see notes for picture.
  --
  -- (>>=) :: (State -> (a,State)) -> (a -> (State -> (b,State))) -> (State -> (b,State))
  --                    h                           f                     result                  
  (MsM h) >>= f = MsM $ \s -> let (a,s') = h s
                                  (MsM g) = f a
                              in g s'

inc :: MSM Int
inc = do s <- get
         set (s {pc = pc s + 1})
         return (pc s)

push :: Int -> MSM ()
push x = do sta <- getStack
            modify (\s -> s {stack = x : sta})


pop :: MSM Int
pop = do sta <- getStack
         modify (\s -> s {stack = tail sta})
         return (head sta)

dup :: MSM Int
dup = do x <- pop
         push x
         push x
         return x

getStack :: MSM Stack
getStack = do s <- get
              return (stack s)

get :: MSM State
get = MsM (\s -> (s,s))

set :: State -> MSM ()
set m = MsM (\s -> ((),m))

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

modify :: (State -> State) -> MSM ()
modify f = do s <- get
              set (f s)
              return ()

neg :: MSM Int
neg = do x <- pop
         push (-x)
         return (-x)

getInst :: MSM Inst
getInst = MsM (\s -> 
          (prog s !! pc s,s))

checkStackunderflow :: MSM Bool
checkStackunderflow = do s <- get
                         if (stack s) /= []
                         then do return True
                         else do return False

interpInst :: Inst -> MSM Bool
interpInst POP      = do t <- checkStackunderflow
                         if t
                         then do pop
                                 inc
                                 return True
                         else do return t

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

interp :: MSM ()
interp = run
  where run = do inst <- getInst
                 cont <- interpInst inst
                 when cont run

runMSM :: Prog -> ((), State)
runMSM p = let (MsM f) = interp 
           in f $ initial p


a = [POP,PUSH 1,HALT]
