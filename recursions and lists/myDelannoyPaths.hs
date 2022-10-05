
delannoyStep :: [[Int]] -> [[Int]] -> [[Int]]
delannoyStep cs ds = [1]: (zipWith3 (\x y z -> map (0 :) x ++ map (2 :) y ++ map (1 :) z) ds (tail ds) cs) ++ [[1]]

delannoyHelper :: [[Int]] -> [[Int]] -> [[[Int]]]
delannoyHelper cs ds = cs: (delannoyHelper ds (delannoyStep cs ds))

delannoyLayers :: [[[Int]]]
delannoyLayers = delannoyHelper [[1]] [[1], [1]]

delannoyPaths :: Int -> Int -> [Int]
delannoyPaths a b = (last $ take (a + b + 1) $ delannoyLayers) !! b