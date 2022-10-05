myUnfoldIntoSyracuse :: Int -> [Int]
myUnfoldIntoSyracuse 1 = [1]
myUnfoldIntoSyracuse x
  | odd x = x : (myUnfoldIntoSyracuse (3 * x + 1))
  | otherwise = x : (myUnfoldIntoSyracuse (x `div` 2))