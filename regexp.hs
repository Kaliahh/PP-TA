import Parsing

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
