import Data.List

sieve :: Int -> [Int] -> [Int]
sieve n (x : xs) = (x : xs) \\ [x, 2 * x .. n]

myUnfoldIntoPrimesHelper :: Int -> [Int] -> [Int]
myUnfoldIntoPrimesHelper _ [] = []
myUnfoldIntoPrimesHelper n (x : xs) = x : (myUnfoldIntoPrimesHelper n (sieve n (x : xs)))

myUnfoldIntoPrimes :: Int -> [Int]
myUnfoldIntoPrimes x = myUnfoldIntoPrimesHelper x [2 .. x]
