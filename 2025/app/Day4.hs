module Day4 where

input :: String
input = "inputs/Day4"

readInput :: FilePath -> IO [String]
readInput fileName = do
    inData <- readFile fileName
    return $ lines inData

markRoll :: [[Char]] -> [[Maybe Int]]
markRoll = map (map (\x -> if x == '@' then Just 0 else Nothing))
checkRoll :: Int -> Int -> [[Maybe Int]] -> Maybe Int
checkRoll row col rolls = let
    handle' Nothing = 0
    handle' (Just _) = 1
    adj = [handle' ((rolls !! r) !! c) |
        r <- [row - 1, row, row + 1],
        r >= 0,
        r<length rolls,
        c <- [col - 1, col, col + 1],
        c < length ( head rolls),
        c >= 0,
        c/=col || r/=row]
    in case rolls !!row !!col of 
        Just _ -> Just (sum adj)
        _ -> Nothing

counted :: [[Maybe Int]] -> [[Maybe Int]]
counted rolls = [[checkRoll i j rolls | j <- [0 .. (length . head $ rolls)-1]] |
    i <- [0 .. length rolls-1]
    ]

countRolls :: [[Maybe Int]] -> Int
countRolls [] = 0
countRolls ([]:rows) = countRolls rows
countRolls ((Nothing:row):rows) = countRolls (row:rows)
countRolls ((Just adj:row):rows) = (if adj < 4 then 1 else 0) + countRolls (row:rows)

removePossible :: [[Maybe Int]] -> [[Maybe Int]]
removePossible [] = []
removePossible ([]:rows) = removePossible rows
removePossible (row:rows) = remove' row : removePossible rows
    where
    remove' :: [Maybe Int] -> [Maybe Int]
    remove' [] = []
    remove' (Nothing:cRow) = Nothing : remove' cRow
    remove' (Just adj:cRow) = (if adj < 4 then Nothing else Just adj) : remove' cRow
removeAll :: [[Maybe Int]] -> Int
removeAll [] = 0
removeAll rolls = let
    countedRolls = counted rolls
    removed = countRolls countedRolls
    new = removePossible countedRolls
    in if removed == 0
        then 0 else
        removed + removeAll new 
-- test
testInput :: [String]
testInput = ["..@@.@@@@.","@@@.@.@.@@","@@@@@.@.@@","@.@@@@..@.","@@.@@@@.@@",".@@@@@@@.@",".@.@.@.@@@","@.@@@.@@@@",".@@@@@@@@.","@.@.@@@.@."]

main :: IO ()
main = do
    rolls <- readInput input
    let marked = markRoll rolls
    print "Part1"
    print $ countRolls . counted $ marked
    print "Part2"
    print $ removeAll marked
