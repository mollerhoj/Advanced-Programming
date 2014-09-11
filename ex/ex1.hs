newtype Trace a = T (a,String) deriving (Show)

instance Monad Trace where
  -- (>>=) :: Trace a -> (a -> Trace b) -> Trace b
  (T (x,s)) >>= f = f x

  -- return :: a -> Trace a
  return x = T(x,"")

traceable :: String -> (t -> a) -> t -> Trace a
traceable name f = \x -> T(f x, name ++ " called.")

newtype Multivalued a = MV [a] deriving (Show)

instance Monad Multivalued where
  -- >>= :: m a -> (a -> m b) -> m b
  MV l >>= f = f (head l)

  -- return :: a -> m a
  return x = MV [x]

sqrtC :: Float -> Multivalued Float
sqrtC x = return x >>= sqrtC
