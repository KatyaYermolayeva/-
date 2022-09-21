myPow :: Int -> Int -> Int
myPow a 1 = a
myPow a b =
  if odd b
  then a * myPow a (b-1)
  else myPow (a*a) (b `div` 2)