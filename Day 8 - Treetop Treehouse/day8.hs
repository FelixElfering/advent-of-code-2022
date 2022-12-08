import Data.Char
import Data.List (transpose)

main = do
    treeGridString <- readFile "./tree_grid.txt"
    let treeGrid = parseTreeGrid $ lines treeGridString
    let treeGridVisibility = map visibleTrees $ rotateGrid90 $ map visibleTrees $ rotateGrid90 $ map visibleTrees $ rotateGrid90 $ map visibleTrees treeGrid
    let visibleTreeCount = length [x | x <- concat treeGridVisibility, snd x]
    print treeGridVisibility
    print visibleTreeCount
    let treeGrid2 = parseTreeGrid2 $ lines treeGridString
    let treeGridViews = treeViews $ rotateGrid90 $ treeViews $ rotateGrid90 $ treeViews $ rotateGrid90 $ treeViews treeGrid2
    let highestScore = maximum [calcScore x | x <- map snd $ concat treeGridViews]
    print treeGridViews
    print highestScore


-- Tree is visible if all trees from an edge are shorter than it

-- Idea: from every angle, list the visible trees

parseTreeGrid :: [String] -> [[(Int, Bool)]]
parseTreeGrid [] = []
parseTreeGrid (x:xs) = [(digitToInt c, False) | c <- x] : parseTreeGrid xs

rotateGrid90 :: [[a]] -> [[a]]
rotateGrid90 xs = transpose $ map reverse xs

visibleTrees :: [(Int, Bool)] -> [(Int, Bool)]
visibleTrees xs =
    let visibleTreesHelper :: [(Int, Bool)] -> Int -> [(Int, Bool)]
        visibleTreesHelper [] _ = []
        visibleTreesHelper (x:xs) acc | fst x > acc = (fst x, True) : visibleTreesHelper xs (fst x)
                                      | fst x <= acc = (fst x, snd x || False) : visibleTreesHelper xs acc
    in
    visibleTreesHelper xs (-1)


-- Part 2 : naive solution: for every tree list all trees in sight and then calculate view score
parseTreeGrid2 :: [String] -> [[(Int, [[Int]])]]
parseTreeGrid2 [] = []
parseTreeGrid2 (x:xs) = [(digitToInt c, []) | c <- x] : parseTreeGrid2 xs


treeViews :: [[(Int, [[Int]])]] -> [[(Int, [[Int]])]] -- view to tree from that side
treeViews = map treesTillTree

treesTillTree :: [(Int, [[Int]])] -> [(Int, [[Int]])]
treesTillTree xs =
    let helper :: [(Int, [[Int]])] -> [Int] -> [(Int, [[Int]])]
        helper [] _ = []
        helper (x:xs) acc = (fst x, [ z | z <- listUntil (\y -> y >= fst x) $ reverse acc] : snd x) : helper xs (acc++[fst x])
    in
        helper xs []

listUntil :: (a -> Bool) -> [a] -> [a]
listUntil f [] = []
listUntil f (x:xs) | f x = [x]
                   | not $ f x = x : listUntil f xs

calcScore :: [[a]] -> Int
calcScore xs = product $ map length xs
