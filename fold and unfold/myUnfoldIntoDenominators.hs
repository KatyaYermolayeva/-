myUnfoldIntoDenominatorsHelper :: Int -> Int -> [Int]
myUnfoldIntoDenominatorsHelper 1 _ = []
myUnfoldIntoDenominatorsHelper x d
  | x `mod` d == 0 = d : myUnfoldIntoDenominatorsHelper (x `div` d) d
  | otherwise = myUnfoldIntoDenominatorsHelper x (d + 1)

myUnfoldIntoDenominators :: Int -> [Int]
myUnfoldIntoDenominators x = myUnfoldIntoDenominatorsHelper x 2
