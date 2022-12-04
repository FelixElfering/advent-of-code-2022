import Data.Char
main = do
    strategyString <- readFile "./strategy.txt"
    let strategies = map words $ lines strategyString
    let finalScore1 = sum [getScoreV1 (head p1) (head p2) | (p1:p2:_) <- strategies]
    let finalScore2 = sum [getScoreV2 (head p1) (head p2) | (p1:p2:_) <- strategies]

    print strategyString
    print strategies
    print finalScore1
    print finalScore2


getScoreV1 :: Char -> Char -> Int
getScoreV1 p1 p2 = 
    p2Val + gameResult
    where
        p1Val = ord p1 - ord 'A' + 1
        p2Val = ord p2 - ord 'X' + 1
        gameResult = gameScore p1Val p2Val

getScoreV2 :: Char -> Char -> Int
getScoreV2 p1 p2 = 
    p2Val + gameResult
    where
        p1Val = ord p1 - ord 'A' + 1
        p2Val = responseMove p1Val p2
        gameResult = gameScore p1Val p2Val

gameScore :: Int -> Int -> Int
gameScore p1 p2 = [3 , 6, 0] !! mod (p2 - p1) 3

responseMove :: Int -> Char -> Int
responseMove p1 'X' = mod (p1 + 1) 3 + 1 --losing move
responseMove p1 'Y' = p1 -- drawing move
responseMove p1 'Z' = mod p1 3 + 1 -- winning move
