module Day2 where

import Data.List.Split
import Text.Read
import Data.Maybe (fromJust)
import Data.List
input :: String
input = "inputs/Day2"
readInput :: String -> IO [String]
readInput fileName = do
    string <- readFile fileName
    return $ splitOn "," string

type ID = String
type Range = (ID,ID)

splitRange :: String -> Range
splitRange s = let sp = splitOn "-" s in
    (head sp, head $ tail sp)

checkID :: ID -> Bool
checkID s = let half = length s `div` 2
    in take half s == drop half s

checkID2 :: ID -> Bool
checkID2 s = elem s . (fmap (take (length s) . cycle) . filter (\x-> not (null x) && length x <= length s `div` 2 && mod (length s) (length x) == 0) . inits) $ s

checkInvalidInRange :: (ID -> Bool) -> Range -> [Int]
checkInvalidInRange c (r1,r2) = checkRange' [i1..i2] where
    i1 = fromJust . readMaybe $ r1
    i2 = fromJust . readMaybe $ r2
    checkRange' :: [Int] -> [Int]
    checkRange' [] = []
    checkRange' (i:xs) = if c (show i) then i : checkRange' xs else checkRange' xs

part1 :: [String] -> Int
part1 = sum . fmap (sum . checkInvalidInRange checkID . splitRange)

part2 :: [String] -> Int
part2 = sum . fmap (sum . checkInvalidInRange checkID2 . splitRange)

-- test
testInput = ["11-22","95-115","998-1012","1188511880-1188511890","222220-222224","1698522-1698528","446443-446449","38593856-38593862","565653-565659","824824821-824824827","2121212118-2121212124"]
main :: IO()
main = do
    rangeStrings <- readInput input
    -- print $ part1 rangeStrings
    print $ part2 rangeStrings
    return ()
