module Day_3 where
import Data.Char (ord)
import Data.List (nub)

priority :: Char -> Int
priority c
        | f >= 97 = ord c - 96
        | f >= 65 = ord c - 38
        | otherwise = -1 -- this should not happen
        where f = ord c

splitHalf :: [Int] -> ([Int], [Int])
splitHalf xs = splitAt (length xs `div` 2) xs

groups :: [[Int]] -> [([Int], [Int], [Int])]
groups [] = []
groups (x:y:z:xs) = (x, y, z) : groups xs
groups _ = error "Input not divisible by 3!"

badge :: ([Int], [Int], [Int]) -> Int
badge (a, b, c) = head $ sharedItems c (sharedItems a b)

sharedItems :: [Int] -> [Int] -> [Int]
sharedItems [] ys = []
sharedItems (x:xs) ys = if x `elem` ys then x : sharedItems xs ys else sharedItems xs ys

rucksackPriorities:: String -> [Int]
rucksackPriorities s = let halfs = splitHalf (map priority s) in nub $ uncurry sharedItems halfs

solution1 :: [String] -> Int
solution1 = sum . map (sum . rucksackPriorities)

solution2 :: [String] -> Int
solution2 s = let g = groups . map (map priority) in sum $ map badge (g s)

prepareInput :: IO [String]
prepareInput = readFile "input" >>= \s -> return $ lines s

main :: IO ()
main = do
        input <- prepareInput
        let sol1 = solution1 input
        print $ "Solution 1: " ++ show sol1
        let sol2 = solution2 input
        print $ "Solution 2: " ++ show sol2

  