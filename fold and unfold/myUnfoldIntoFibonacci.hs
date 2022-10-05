myUnfoldIntoFibonacciHelper :: Int -> Int -> Int -> [Int]
myUnfoldIntoFibonacciHelper 0 _ _ = []
myUnfoldIntoFibonacciHelper n x y = x : myUnfoldIntoFibonacciHelper (n - 1) y (x + y)

myUnfoldIntoFibonacci :: Int -> [Int]
myUnfoldIntoFibonacci n = myUnfoldIntoFibonacciHelper n 0 1

myUnfoldIntoFibonacciInfHelper :: Int -> Int -> [Int]
myUnfoldIntoFibonacciInfHelper x y = x : myUnfoldIntoFibonacciInfHelper y (x + y)
myUnfoldIntoFibonacciInf :: [Int]
myUnfoldIntoFibonacciInf = myUnfoldIntoFibonacciInfHelper 0 1
