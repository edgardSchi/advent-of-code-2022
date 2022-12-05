module Day_4 where

-- Idea from https://stackoverflow.com/a/7569301
splitBy :: Char -> String -> [String]
splitBy _   [] = []
splitBy del s  = foldr f [""] s
    where f :: Char -> [String] -> [String]
          f _ [] = []
          f c string@(x:xs)
            | c == del = "":string
            | otherwise = (c:x):xs

boolRep :: (Int, Int) -> [Bool]
boolRep (min, max) = f 1 100 [min..max] []
  where f :: Int -> Int -> [Int] -> [Bool] -> [Bool]
        f index max [] bs = if index < max then f (index+1) max [] (False:bs) else bs
        f index max (x:xs) bs = if index == x then f (index+1) max xs (True:bs) else f (index+1) max (x:xs) (False:bs)

readRanges :: String -> (Int, Int)
readRanges string = let (min:max:s) = splitBy '-' string in (read min :: Int, read max ::Int)

toListRanges :: String -> ([Bool], [Bool])
toListRanges string = let (left:right:s) = splitBy ',' string in
                      let t = boolRep . readRanges in
                      (t left, t right)

contains :: ([Bool], [Bool]) -> Bool
contains (l, r) = let andList = zipWith (&&) l r in
                  andList == l || andList == r

overlaps :: ([Bool], [Bool]) -> Bool
overlaps (l, r) = or $ zipWith (&&) l r

prepareInput :: IO [String]
prepareInput = readFile "input" >>= \s -> return $ lines s

solution :: (([Bool], [Bool]) -> Bool) -> [String] -> Int
solution f = foldr ((\b i -> if b then i+1 else i) . f . toListRanges) 0

solution1 :: [String] -> Int
solution1 = solution contains

solution2 :: [String] -> Int
solution2 = solution overlaps

main = do
        input <- prepareInput
        let sol1 = solution1 input
        print $ "Solution 1: " ++ show sol1
        let sol2 = solution2 input
        print $ "Solution 2: " ++ show sol2