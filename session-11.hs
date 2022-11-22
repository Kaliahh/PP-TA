import Parsing

data Onion = Core Int | Layer Onion
  deriving (Show)

theonion :: Parser Onion
theonion =
  do
    char 'L'
    x <- theonion
    return (Layer x)
    <|> do
      x <- integer
      return (Core x)

eval :: Parser a -> String -> a
eval p xs = case parse p xs of
  [(n, [])] -> n
  [(_, out)] -> error ("Unused input " ++ out)
  [] -> error "Invalid input"

ab :: Parser String
ab = do
  as <- many (do char 'a')
  bs <- many (do char 'b')
  if length as == length bs then return "OK" else return []

-- R ::= a | b | R1 . R2 | R1 U R2 | (R1)*

-- R -> T . R | T U R | T
-- T -> B* | B
-- B -> a | b | (R)

reg :: Parser String
reg =
  do
    t <- ter
    symbol "."
    reg
    <|> do
      t <- ter
      symbol "U"
      reg
    <|> do
      ter

ter :: Parser String
ter =
  do
    bas
    symbol "*"
    <|> do
      bas

bas :: Parser String
bas =
  do
    symbol "a"
    <|> do
      symbol "b"
    <|> do
      symbol "("
      reg
      symbol ")"

data Rexp = A | B | Union Rexp Rexp | Star Rexp | Conc Rexp Rexp deriving (Show)

regexp :: Parser Rexp
regexp =
  do
    t <- term
    symbol "."
    r <- regexp
    return (Conc t r)
    <|> do
      t <- term
      symbol "U"
      r <- regexp
      return (Union t r)
    <|> do
      t <- term
      return t

term :: Parser Rexp
term =
  do
    b <- base
    -- case b of
    --   A -> many (do symbol "a")
    --   B -> many (do symbol "b")
    --   _ -> many (do base)
    symbol "*"
    return (Star b)
    <|> do
      b <- base
      return b

base :: Parser Rexp
base =
  do
    symbol "a"
    return A
    <|> do
      symbol "b"
      return B
    <|> do
      symbol "("
      r <- regexp
      symbol ")"
      return r
