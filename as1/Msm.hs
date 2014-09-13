module Msm
( Inst (..)
, ErrorType (..)
, Error (..)
, runMSM
) where
-- I would have liked to only export runMSM, Error and Prog.
-- But I could not figure out how. Instead, I export their internal representation.

import Control.Monad
import qualified Data.Map as Map

type Stack = [Int] 

-- My registers have a(n unusual property): They can contain either nothing or
-- (just) an Int, or they can of course be unallocated. If we try to load from
-- an allocated register, where nothing is stored, we get an
-- "Unspec "register empty" error. See test 24 for an example
type Regs = Map.Map Int (Maybe Int)
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
              reg = Map.empty
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
die tx = MsM (\_ -> Left Error {t = tx})

push :: Int -> MSM ()
push x = do s <- get
            set (s {stack = x : stack s})

pop :: MSM Int
pop = do s <- get
         if stack s /= []
         then do set (s {stack = tail (stack s)})
                 return (head (stack s))
         else die StackUnderflow

retrive :: Int -> MSM (Maybe (Maybe Int))
retrive i = do s <- get
               return $ Map.lookup i (reg s)

dup :: MSM Int
dup = do x <- pop
         push x
         push x
         return x

get :: MSM State
get = MsM (\s -> Right (s,s))

set :: State -> MSM ()
set m = MsM (\_ -> Right ((),m))

add :: MSM Int
add = do x <- pop
         y <- pop
         let z = x+y
         push z
         return z

-- required by exercise? But I like using get/set more...
--modify :: (State -> State) -> MSM ()
--modify f = do s <- get
--              set (f s)
--              return ()

neg :: MSM Int
neg = do x <- pop
         push (-x)
         return (-x)

getInst :: MSM Inst
getInst = do s <- get
             if inRange (pc s) (prog s)
             then return (prog s !! pc s)
             else die InvalidPC

-- Is the index inside the list? I don't know the standard library very well,
-- but this might already be there..
inRange :: Int -> [a] -> Bool
inRange i l = i >= 0 && i < length l

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

interpInst (NEWREG i)= do p <- retrive i
                          case p of
                            Just _  -> die RegisterAlreadyAllocated
                            Nothing -> do s <- get
                                          set (s {reg = Map.insert i Nothing (reg s)})
                                          inc
                                          return True

interpInst (STORE)   = do m <- pop
                          n <- pop
                          p <- retrive n
                          case p of
                            Nothing -> die (UnallocatedRegister n)
                            Just _ -> do s <- get
                                         set (s {reg = Map.insert n (Just m) (reg s)})
                                         inc
                                         return True

interpInst (LOAD)   = do  i <- pop
                          p <- retrive i
                          case p of
                            Nothing -> die (UnallocatedRegister i)
                            Just x -> case x of
                                        Nothing -> die (Unspec "Register empty")
                                        Just y  -> do push y
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
                                             []      -> Left Error {t = StackUnderflow}
                                             (x:_)  -> Right x

runMSMHelper :: Prog -> Either Error ((), State)
runMSMHelper p = let (MsM f) = interp 
           in f $ initial p
