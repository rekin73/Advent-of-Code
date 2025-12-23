module Day3 where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

input :: String
input = "inputs/Day3"

readInput :: String -> IO [String]
readInput fileName = do
  string <- readFile fileName
  return $ lines string

getMaxJ :: String -> String
getMaxJ [] = ""
getMaxJ [b] = ['0', b]
getMaxJ (b : bs) = max [b, maximum bs] (getMaxJ bs)

getMaxJN :: Int -> String -> String
getMaxJN _ [] = ""
getMaxJN 0 _ = ""
getMaxJN 1 bs = [maximum bs]
getMaxJN n bs =
  let l = length bs
      initl = l - n + 1
      takeInit = take initl bs
      (mxb, afterMxb) = getMaxInit' ('0', []) takeInit
      getMaxInit' res [] = res
      getMaxInit' (cChar, cRest) (cb : rest)
        | cb > cChar = getMaxInit' (cb, rest) rest
        | otherwise = getMaxInit' (cChar, cRest) rest
   in mxb : getMaxJN (n - 1) (afterMxb ++ drop initl bs)

part1 :: [String] -> Int
part1 = sum . mapMaybe (readMaybe . getMaxJ)

part1new :: [String] -> Int
part1new = sum . mapMaybe (readMaybe . getMaxJN 2)

part2 :: [String] -> Int
part2 = sum . mapMaybe (readMaybe . getMaxJN 12)

-- test
testInput = ["987654321111111", "811111111111119", "234234234234278", "818181911112111"]

main :: IO ()
main = do
  inLines <- readInput input
  print $ part1 inLines
  print $ part1new inLines
  print $ part2 inLines
