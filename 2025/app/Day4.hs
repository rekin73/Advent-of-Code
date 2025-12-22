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

counted :: [[Maybe Int]] -> [Maybe Int]
counted rolls = [checkRoll i j rolls |
    i <- [0 .. length rolls-1],
    j <- [0 .. (length . head $ rolls)-1]]

countRolls :: [[Maybe Int]] -> Int
countRolls rolls = length . filter (maybe False (< 4)) $ counted rolls
-- test
testInput :: [String]
testInput = ["..@@.@@@@.","@@@.@.@.@@","@@@@@.@.@@","@.@@@@..@.","@@.@@@@.@@",".@@@@@@@.@",".@.@.@.@@@","@.@@@.@@@@",".@@@@@@@@.","@.@.@@@.@."]

main :: IO ()
main = do
    rolls <- readInput input
    print $ countRolls . markRoll $ rolls
