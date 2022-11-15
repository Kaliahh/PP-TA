tuple :: Monad m => m a -> m b -> m (a, b)
tuple x y =
  x >>= \o ->
    y >>= \k ->
      return (o, k)

tuple' :: Monad m => m a -> m b -> m (a, b)
tuple' x y =
  do
    o <- x
    k <- y
    return (o, k)

fourfirst xs = do
  x <- xs
  return (4, x)

-- OPGAVE 2:

-- Defining states and state transformers

newtype State = SE [Int] deriving (Show)

newtype ST a = ST (State -> (a, State))

app :: ST a -> State -> (a, State)
app (ST st) x = st x

-- Defining the ST monad

-- We must first make ST a functor

instance Functor ST where
  -- fmap :: (a->b) -> ST(a->b)
  fmap g st = ST (\s -> let (x, s') = app st s in (g x, s'))

-- Then we must make ST an applicative functor

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = ST (\s -> (x, s))

  -- <*> :: ST (a->b) -> ST a -> ST b

  stf <*> stx =
    ST
      ( \s ->
          let (f, s') = app stf s
              (x, s'') = app stx s'
           in (f x, s'')
      )

-- Finally, we can make ST a monad; we only need to define >>= as return is simply the -- pure function

instance Monad ST where
  -- >>= :: a -> (a -> ST b) -> ST b
  st >>= f =
    ST
      ( \s ->
          let (x, s') = app st s in app (f x) s'
      )

get :: ST [Int]
get = ST (\s -> let (SE se) = s in (se, s))

get' :: ST Int
get' = ST (\s -> let (SE (x : xs)) = s in (x, s))

-- remove = ST (\s -> case s of
--                         (SE []) -> (0,(SE []))
--                         (SE (x:xs)) -> (x,(SE xs)))

remove = ST (\s -> pop s)
  where
    pop :: State -> (Int, State)
    pop (SE []) = (0, (SE []))
    pop (SE (x : xs)) = (x, (SE xs))

put x = ST (\s -> let SE l = s in (x, SE (x : l)))

-- OPGAVE 3:

push :: Int -> ST Int
push y = do
  put y

pop :: ST Int
pop = do
  remove

add :: ST Int
add = do
  x <- pop
  y <- pop
  push (x + y)

mult :: ST Int
mult = do
  x <- pop
  y <- pop
  push (x * y)

myprog :: ST Int
myprog = do
  push 2
  push 3
  add
  push 5
  mult

mystate = SE [0]

-- BOGSTAVOPGAVEN

-- a:

foldM :: Monad m => (t1 -> t2 -> m t2) -> [t1] -> t2 -> m t2
foldM f [] v = return v
foldM f (x : xs) v = do
  w <- f x v
  foldM f xs w

dingo x = do
  putStrLn (show x)
  return x
