module Day1 (main) where

import Data.Maybe
import Text.Read (readMaybe)

-- Read and Process functions
readInput :: String -> IO [String]
readInput fileName = do
  string <- readFile fileName
  return $ lines string

data Move = L Int | R Int
  deriving (Show)

parseMove :: String -> Maybe Move
parseMove [] = Nothing
parseMove (x : xs) =
  let rotNum = readMaybe xs
      m | x == 'L' = Just L | x == 'R' = Just R | otherwise = Nothing
   in m <*> rotNum

-- The Functions
move :: Int -> Move -> Int
move position (L i) = mod (position - i) 100
move position (R i) = mod (position + i) 100

getLockval :: [a] -> Int -> a
getLockval lock position = lock !! position

movesToVals :: [Move] -> [a] -> Int -> [a]
movesToVals [] _ _ = []
movesToVals (m : moves) lock start = getLockval lock newPos : movesToVals moves lock newPos
  where
    newPos = move start m

-- Part2
count0Clicks :: Int -> Move -> Int
count0Clicks pos (R i) =
  let fullRot = div i 100
      -- more0 = div (pos + mod i 100) 100
      more0 = if (pos + mod i 100) >= 100 then 1 else 0
   in fullRot + more0
count0Clicks pos (L i) =
  let fullRot = div i 100
      more0 = if (pos - mod i 100) <= 0 && pos /= 0 then 1 else 0
   in fullRot + more0

seqCount :: [Move] -> Int -> [(Int, Int)]
seqCount [] _ = []
seqCount (m : moves) pos = (newPos, rotCount) : seqCount moves (move pos m)
  where
    newPos = move pos m
    rotCount = count0Clicks pos m

-- test
-- testMovesString :: [String]
-- testMovesString = ["L68","L30","R48","L5","R60","L55","L1","L99","R14","L82"]
-- testMoves :: [Move]
-- testMoves = mapMaybe parseMove testMovesString
-- input file
input :: String
input = "../inputs/Day1"

main :: IO ()
main = do
  readLines <- readInput input
  let moves = mapMaybe parseMove readLines
  let lock = [0 .. 99]
  let vals = movesToVals moves lock 50
  let part2Sol = sum $ snd <$> seqCount moves 50
  print "Part1: "
  print $ length $ filter (== (0 :: Integer)) vals
  print "Part2: "
  print part2Sol
