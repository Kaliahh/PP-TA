import Data.Maybe

data Onion a = Core a | Layer (Onion a)
  deriving (Show)

instance Functor Onion where
  -- fmap :: (a -> b) -> Onion a -> Onion b
  fmap g (Core x) = Core (g x)
  fmap g (Layer x) = Layer (fmap g x)

data UTree a = Node a [UTree a]
  deriving (Show)

instance Functor UTree where
  -- fmap :: (a -> b) -> UTree a -> UTree b
  fmap g (Node x xs) = Node (g x) (fmap (fmap g) xs)

-- instance Functor ((->) a) where
-- fmap (a -> b) -> (r -> a) -> (r -> b)
-- fmap = (.)

(<*>) :: [a -> b] -> [a] -> [b]
[] <*> _ = []
_ <*> [] = []
(f : fs) <*> xs = fmap f xs ++ (fs Main.<*> xs)

data Exp a = Var a | Val Int | Add (Exp a) (Exp a)

instance Functor Exp where
  -- fmap :: (a -> b) -> Exp a -> Exp b
  fmap g (Var a) = Var (g a)
  fmap g (Val x) = Val x
  fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Exp where
  -- pure a -> Exp a
  pure x = Var x

  -- <*> :: Exp (a -> b) -> Exp a -> Exp b
  _ <*> Val x = Val x
  Val x <*> _ = Val x
  Var f <*> Var x = Var (f x)
  Var f <*> Add x y = Add (fmap f x) (fmap f y)
  (Add f g) <*> x = Add (f Prelude.<*> x) (g Prelude.<*> x)