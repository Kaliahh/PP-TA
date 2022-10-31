hello :: IO ()
hello = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Hello " ++ name ++ "!")

n = sequence_ [putStr "rip", putStr "rap", return ()]

scramble = do
  w <- getLine
  loop (read w :: Int)
  where
    loop 1 = print 1
    loop x = do
      putStr (show x)
      if even x
        then loop (div x 2)
        else loop (x + 1)

letter = do
  putStrLn "Write the string:"
  s <- getLine
  printEach s
  return ()
  where
    printEach [] = do return ()
    printEach (x : xs) = do
      putChar x
      putChar '\n'
      printEach xs

letter' = do
  putStrLn "Write the string:"
  s <- getLine
  sequence_ [putStrLn [x] | x <- s]

letter'' = do
  putStrLn "Write the string:"
  s <- getLine
  mapM_ putStrLn ([[x] | x <- s])

hugorm = do
  putStr "How many numbers would you like to add?: "
  numberString <- getLine
  tallyUp (read numberString :: Int) 0
  where
    tallyUp 0 tally = do
      putStrLn ("The sum is " ++ show tally)
    tallyUp x tally = do
      number <- getLine
      tallyUp (x - 1) (tally + (read number :: Int))

getInt = do
  s <- getLine
  return (read s :: Int)

sumInts input = do
  i <- getInt
  if i == 0
    then return input
    else sumInts (i + input)

-- whileIO :: IO a -> (a -> Bool) -> (a -> a -> a) -> a -> IO a

whileIO :: (Monad m, Read t1) => m String -> (String -> Bool) -> (t2 -> t1 -> t2) -> t2 -> m t2
whileIO ioAction stopCond fld acc = do
  w <- ioAction
  let x = read w
   in if stopCond w
        then return acc
        else whileIO ioAction stopCond fld (fld acc x)