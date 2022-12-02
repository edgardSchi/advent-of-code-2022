module Day_2 where

data RPS = Rock | Paper | Scissors deriving Show
data Outcome = Win | Loss | Draw deriving Show

decodeMove :: String -> RPS
decodeMove "A" = Rock
decodeMove "B" = Paper
decodeMove "C" = Scissors
decodeMove "X" = Rock
decodeMove "Y" = Paper
decodeMove "Z" = Scissors
decodeMove _   = undefined

decodeExpectedMove :: String -> Outcome
decodeExpectedMove "X" = Loss
decodeExpectedMove "Y" = Draw
decodeExpectedMove "Z" = Win
decodeExpectedMove _   = undefined

scoreShape :: RPS -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

outcome :: RPS -> RPS -> Outcome
outcome Rock Rock = Draw
outcome Rock Paper = Win
outcome Rock Scissors = Loss
outcome Paper Rock = Loss
outcome Paper Paper = Draw
outcome Paper Scissors = Win
outcome Scissors Rock = Win
outcome Scissors Paper = Loss
outcome Scissors Scissors = Draw

expectedMove :: RPS -> Outcome -> RPS
expectedMove Rock Win = Paper
expectedMove Rock Draw = Rock
expectedMove Rock Loss = Scissors
expectedMove Paper Win = Scissors
expectedMove Paper Draw = Paper
expectedMove Paper Loss = Rock
expectedMove Scissors Win = Rock
expectedMove Scissors Draw = Scissors
expectedMove Scissors Loss = Paper

scoreOutcome :: Outcome -> Int
scoreOutcome Win = 6
scoreOutcome Draw = 3
scoreOutcome Loss = 0

prepareInput :: IO [[String]]
prepareInput = readFile "input" >>= \s -> return $ (map words . lines) s

decodeInput1 :: [[String]] -> [[RPS]]
decodeInput1 = map (map decodeMove)

decodeInput2 :: [[String]] -> [[RPS]]
decodeInput2 = map (\[x,y] -> let opMove = decodeMove x in [opMove, expectedMove opMove (decodeExpectedMove y)])

roundOutcome :: [RPS] -> Int
roundOutcome xs = scoreShape (xs!!1) + scoreOutcome (outcome (xs!!0) (xs!!1))

overallOutcome :: [[RPS]] -> Int
overallOutcome = sum . map roundOutcome

main :: IO ()
main = do
        input <- prepareInput
        let strategy = decodeInput1 input
        let sol1 = overallOutcome strategy
        print $ "Solution 1: " ++ show sol1
        let strategy = decodeInput2 input
        let sol2 = overallOutcome strategy
        print $ "Solution 2: " ++ show sol2