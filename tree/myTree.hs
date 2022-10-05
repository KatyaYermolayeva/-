data MyTree a
    = XNil               
    | XCons a (MyTree a) (MyTree a)

xMap :: (a -> b) -> MyTree a -> MyTree b
xMap _ XNil = XNil
xMap f (XCons x a b) = XCons (f x)  (xMap f a)  (xMap f b)

xSize :: MyTree a -> Int
xSize XNil = 0
xSize (XCons x a b) = 1 + xSize a + xSize b

treeTraverseD :: (a -> b -> b) -> b -> MyTree a -> b
treeTraverseD _ b XNil = b
treeTraverseD f b (XCons x left right) = treeTraverseD f (f x (treeTraverseD f b left)) right

foldMyTrees :: (a -> b -> b) -> b -> [MyTree a] -> b
foldMyTrees _ b [] = b
foldMyTrees f b (XNil : xs) = foldMyTrees f b xs
foldMyTrees f b ((XCons a left right) : xs) = foldMyTrees f (f a b) xs

getChildNodesOfTree :: MyTree a -> [MyTree a]
getChildNodesOfTree XNil = []
getChildNodesOfTree (XCons x left right) = [left, right]

getChildNodesOfTrees :: [MyTree a] -> [MyTree a]
getChildNodesOfTrees [] = []
getChildNodesOfTrees (x : xs) = (getChildNodesOfTree x) ++ (getChildNodesOfTrees xs)

treeTraverseWHelper :: (a -> b -> b) -> b -> [MyTree a] -> b
treeTraverseWHelper _ b [] = b
treeTraverseWHelper f b trees = 
  let newb = foldMyTrees f b trees
      newtrees = getChildNodesOfTrees trees
  in treeTraverseWHelper f newb newtrees

treeTraverseW :: (a -> b -> b) -> b -> MyTree a -> b
treeTraverseW f b x = treeTraverseWHelper f b [x]