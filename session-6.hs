data Unary = Z | I Unary
  deriving (Show)

four :: Unary
four = I (I (I (I Z)))

unary2int :: Unary -> Int
unary2int Z = 0
unary2int (I n) = 1 + unary2int n

data Tree a = Leaf a | Node (Tree a) a (Tree a) | Empty
  deriving (Show)

myTree :: Tree Integer
myTree = Node (Node (Leaf 4) 2 (Leaf 5)) 1 (Node (Leaf 6) 3 (Leaf 7))

least :: (Ord a) => Tree a -> a
least (Leaf x) = x
least (Node x n y) = minimum [l, n, r]
  where
    l = least x
    r = least y

data Aexp = N Int | V String | Plus Aexp Aexp | Mult Aexp Aexp

type Assoc k v = [(k, v)]

find :: (Eq k) => k -> Assoc k v -> v
find k ass = head [v | (k', v) <- ass, k' == k]

eval :: Aexp -> Assoc String Int -> Int
eval (N n) _ = n
eval (V v) ass = find v ass
eval (Plus x y) ass = n1 + n2
  where
    n1 = eval x ass
    n2 = eval y ass
eval (Mult x y) ass = n1 * n2
  where
    n1 = eval x ass
    n2 = eval y ass

myExpression :: Aexp
myExpression = Plus (Mult (N 2) (V "x")) (V "y")

myAssoc :: Assoc String Int
myAssoc = [("x", 3), ("y", 4)]

data Dir = File String Int | Directory String [Dir]

mySearchTree :: Tree Integer
mySearchTree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

insert :: Ord a => Tree a -> a -> Tree a
insert Empty n = Leaf n
insert (Leaf x) n
  | n < x = Node (Leaf n) x Empty
  | n > x = Node Empty x (Leaf n)
  | otherwise = Leaf x
insert (Node l x r) n
  | n < x = Node l' x r
  | n > x = Node l x r'
  | otherwise = Node l x r
  where
    l' = insert l n
    r' = insert r n

numLeaves :: Tree a -> Int
numLeaves Empty = 0
numLeaves (Leaf _) = 1
numLeaves (Node l _ r) = n1 + n2
  where
    n1 = numLeaves l
    n2 = numLeaves r

unbalancedTree :: Tree Int
unbalancedTree = Node (Node (Leaf 1) 1 (Leaf 1)) 2 (Leaf 1)

strictBalanced :: Tree a -> Bool
strictBalanced Empty = False
strictBalanced (Leaf _) = True
strictBalanced (Node l _ r)
  | strictBalanced l && strictBalanced r && numLeaves l == numLeaves r = True
  | otherwise = False

balanced' (Node t1 _ t2) =
  let v1 = numLeaves t1
      v2 = numLeaves t2
      diff = (abs v1 - v2)
   in (diff <= 1) && balanced' t1 && balanced' t2
balanced' x = True

data Expr = Val Int | Add Expr Expr

foldexp :: (Int -> a) -> (a -> a -> a) -> Expr -> a
foldexp f g (Val a) = f a
foldexp f g (Add a b) = g v1 v2
  where
    v1 = foldexp f g a
    v2 = foldexp f g b

-- eval' :: Expr -> Int
-- eval' e = foldexp

build :: Ord a => [a] -> Tree a
build = foldl insert Empty

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

treesort :: Ord a => [a] -> [a]
treesort list = flatten (build list)
