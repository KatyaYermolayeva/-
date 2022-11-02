delannoyPathsStep :: [[[Int]]] -> [[[Int]]] -> [[[Int]]]
delannoyPathsStep cs ds = (map (flip (++) [2]) (head ds)) : (zipWith3 (\x y z -> (map (flip (++) [0]) x) ++ (map (flip (++) [2]) y) ++ (map (flip (++) [1]) z)) ds (tail ds) cs) ++ [(map (flip (++) [0]) (last ds))]

delannoyPathsHelper :: [[[Int]]] -> [[[Int]]] -> [[[[Int]]]]
delannoyPathsHelper cs ds = cs : (delannoyPathsHelper ds (delannoyPathsStep cs ds))

delannoyPathsLayers :: [[[[Int]]]]
delannoyPathsLayers = delannoyPathsHelper [[[]]] [[[2]], [[0]]]

delannoyPaths :: Int -> Int -> [[Int]]
delannoyPaths a b = (last $ take (a + b + 1) $ delannoyPathsLayers) !! b