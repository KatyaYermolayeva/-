myUnfoldIntoDigits :: Int -> [Int]
myUnfoldIntoDigits 0 = []
myUnfoldIntoDigits x = (x `mod` 2) : (myUnfoldIntoDigits (x `div` 2))