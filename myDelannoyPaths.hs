delannoyPaths :: Int -> Int -> [[Int]]
delannoyPaths 0 0 = [[]]
delannoyPaths n 0 = [replicate n 0]
delannoyPaths 0 n = [replicate n 2]
delannoyPaths a b = map (2 :) (delannoyPaths a (b - 1))) ++ (map (0 :) (delannoyPaths (a - 1) b)) ++ (map (1 :) (delannoyPaths (a - 1) (b - 1))