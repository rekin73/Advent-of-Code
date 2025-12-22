module Day4 where

input :: String
input = "inputs/Day4"

readInput :: FilePath -> IO [String]
readInput fileName = do
    inData <- readFile fileName
    return $ lines inData
testAdj :: Foldable t => Int -> Int -> [t a] -> [(Int, Int)]
testAdj row col rolls= [(r,c)|
    r <- [row - 1, row, row + 1],
    r >= 0,
    r<length rolls,
    c <- [col - 1, col, col + 1],
    c < length ( head rolls),
    c >= 0, c/=r || (c==r) && c/=col && r/=row ]

checkRoll :: Int -> Int -> [[Char]] -> Int
checkRoll row col rolls = let
    adj = [(\ x -> if x == '@' then 1 else 0) ((rolls !! r) !! c) |
        r <- [row - 1, row, row + 1],
        r >= 0,
        r<length rolls,
        c <- [col - 1, col, col + 1],
        c < length ( head rolls),
        c >= 0, c/=r || (c==r) && c/=col && r/=row ]
    in
    sum adj

counted :: [[Char]] -> [Int]
counted rolls = [checkRoll i j rolls |
    i <- [0 .. length rolls-1],
    j <- [0 .. (length . head $ rolls)-1]]

countRolls :: [[Char]] -> Int
countRolls rolls = length . filter (<4) $ counted rolls
-- test
testInput :: [String]
testInput = ["..@@.@@@@.","@@@.@.@.@@","@@@@@.@.@@","@.@@@@..@.","@@.@@@@.@@",".@@@@@@@.@",".@.@.@.@@@","@.@@@.@@@@",".@@@@@@@@.","@.@.@@@.@."]

main :: IO ()
main = do
    rolls <- readInput input
    print $ countRolls rolls
