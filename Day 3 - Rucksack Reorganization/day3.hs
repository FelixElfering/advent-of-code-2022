import Data.List
import Data.Char (ord)
main = do
    rucksackContentsString <- readFile "./rucksack_contents.txt"
    let rucksackContents = lines rucksackContentsString
    let wrongItems = [head $ repeatedChars s1 s2 | (s1, s2) <- map splitInHalf rucksackContents]
    let prioritySum = sum $ map itemValue wrongItems

    print wrongItems
    print prioritySum

    let elfGroups = splitInto3s rucksackContents
    let commonGroupItem = map commonItem elfGroups
    let priorityBadgeSum = sum $ map itemValue commonGroupItem

    print elfGroups
    print commonGroupItem
    print priorityBadgeSum


splitInHalf :: String -> (String, String)
splitInHalf s = splitAt (length s `quot` 2) s

repeatedChars :: String -> String -> String
repeatedChars s1 s2 = 
    let repeatedCharHelper x y | null x || null y = ""
                               | head x == head y = head x : repeatedCharHelper (tail x) (tail y)
                               | head x < head y = repeatedCharHelper (tail x) y
                               | x > y = repeatedCharHelper x (tail y)
                               | otherwise = ""

    in
        repeatedCharHelper sortedS1 sortedS2

    where
        sortedS1 = sort s1
        sortedS2 = sort s2

itemValue :: Char -> Int
itemValue c
    | c `elem` ['a'..'z'] = ord c - ord 'a' + 1
    | c `elem` ['A'..'Z'] = ord c - ord 'A' + 27

splitInto3s :: [String] -> [(String, String, String)]
splitInto3s xs = 
    let helper :: [String] -> [String] -> [(String, String, String)]
        helper [] (x1:x2:x3:_) = [(x1, x2, x3)]
        helper xs (x1:x2:x3:_) = (x1, x2, x3) : helper xs []
        helper (x:xs) acc = helper xs (x:acc)
    in
        helper xs []

commonItem :: (String, String, String) -> Char
commonItem (x1, x2, x3) = head $ repeatedChars x3 $ repeatedChars x1 x2
