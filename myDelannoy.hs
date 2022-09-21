delannoy :: Int -> Int -> Int
delannoy a 0 = 1
delannoy 0 b = 1
delannoy a b = delannoy (a - 1) b + delannoy a (b - 1) + delannoy (a - 1) (b - 1)