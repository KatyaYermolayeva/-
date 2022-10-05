myUnfoldIntoSmaller :: Int -> [Int]
myUnfoldIntoSmaller x
  | x <= 0 = []
  | otherwise = (x - 1) : (myUnfoldIntoSmaller (x - 1))