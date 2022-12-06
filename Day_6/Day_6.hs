module Day_6 where
import Data.List (nub)
import Data.Maybe (fromMaybe)

marker :: String -> Int -> Int
marker s markerSize = f s markerSize
    where f :: String -> Int -> Int
          f string i = if nub (take markerSize string) == take markerSize string then i else f (tail string) (i+1)

readInput :: IO String
readInput = readFile "input"

solution1 :: String -> Int
solution1 s = marker s 4

solution2 :: String -> Int
solution2 s = marker s 14

main = do
    input <- readInput
    let sol1 = solution1 input
    print $ "Solution 1: " ++ show sol1
    let sol2 = solution2 input
    print $ "Solution 2: " ++ show sol2