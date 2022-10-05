myFoldFromDigitsHelper :: [Int] -> Int -> Int -> Int
myFoldFromDigitsHelper [] _ s = s
myFoldFromDigitsHelper (x : xs) y s = myFoldFromDigitsHelper xs (y * 2) (s + y * x)

myFoldFromDigits :: [Int] -> Int
myFoldFromDigits x = myFoldFromDigitsHelper x 1 0