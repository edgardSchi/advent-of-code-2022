module Day_1 where
import Data.List (sortBy)

prepareInput :: IO [String]
prepareInput = readFile "input" >>= \s -> return $ lines s

group :: String -> [String] -> [[String]]
group _ [] = []
group del xs = let s = span (del /=) xs in fst s : group del (drop 1 $ snd s)

convertToInt :: [[String]] -> [[Int]]
convertToInt = map (map (\x -> read x :: Int))

solution1 :: [[Int]] -> Int
solution1 = maximum . map sum

solution2 :: [[Int]] -> Int
solution2 = sum . take 3 . sortBy (flip compare) . map sum

main :: IO ()
main = do
        input <- prepareInput
        let intList = (convertToInt . group "") input
        let sol1 = solution1 intList
        print $ "Solution 1: " ++ show sol1
        let sol2 = solution2 intList
        print $ "Solution 2: " ++ show sol2

