second :: [a] -> Maybe a
second [] = Nothing
second [x] = Nothing
second (_:y:rest) = Just y

second' :: [a] -> a
second' [] = error "Empty list!"
second' [x] = error "No second element!"
second' (_:y:_) = y

second'' :: [a] -> a
second'' [] = error "Empty list!"
second'' (_ : rest) = head rest

second''' :: [a] -> a
second''' list = head (tail list)

second'''' :: [a] -> a
second'''' list = list !! 1

secondInList :: [a] -> [a]
secondInList [] = []
secondInList [x] = []
secondInList (_:y:_) = [y]

secondInList' :: [a] -> [a]
secondInList' [] = []
secondInList' [x] = []
secondInList' list = [head (tail list)]

allbutsecond :: [a] -> [a]
allbutsecond [] = []
allbutsecond [x] = [x]
allbutsecond (x:_:rest) = x : rest

allbutsecond' :: [a] -> [a]
allbutsecond' [] = []
allbutsecond' [x] = [x]
allbutsecond' list = head list : tail (tail list)

midtover :: [a] -> Int -> ([a], [a])
midtover list n = splitAt half list
  where half = div n 2

midtover' :: [a] -> ([a], [a])
midtover' list = splitAt half list
  where half = div (length list) 2

midtover'' :: [a] -> Int -> ([a], [a])
midtover'' list n = (take half list, drop half list)
  where half = div n 2

last' :: [a] -> a
last' list = head (reverse list)