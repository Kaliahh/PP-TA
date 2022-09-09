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

secondinlist :: [a] -> [a]
secondinlist [] = []
secondinlist [x] = []
secondinlist (_:y:_) = [y]

allbutsecond :: [a] -> [a]
allbutsecond [] = []
allbutsecond [x] = [x]
allbutsecond (x:_:rest) = x : rest

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
