module Day_5 where

type Stack = [Char]
-- (Amount, From, To)
type Instruction = (Int, Int, Int)

push :: Char -> Stack -> Stack
push = (:)

-- First argument is stack to be put on top of second argument
pushM :: Stack -> Stack -> Stack
pushM [] bs = bs
pushM (a:as) bs = pushM as (a:bs)

-- pushM that keeps the order
pushMKO :: Stack -> Stack -> Stack
pushMKO = (++)

pop :: Stack -> (Char, Stack)
pop s = (head s, tail s)

-- First part is the popped stack, second is the resulting stack
popM :: Int -> Stack -> (Stack, Stack)
popM = splitAt

-- Hard coding the input because I do not have the time to write a parser for the input format (shame on me)
initStacks :: [Stack]
initStacks = ["WPGZVSB", "FZCBVJ", "CDZNHMLV", "BJFPZMDL", "HQBJGCFV", "BLSTQFG", "VZCGL", "GLN", "CHFJ"]

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

readInstructions :: IO [String]
readInstructions = readFile "input" >>= \s -> return $ drop 10 (lines s)

decodeInstr :: String -> Instruction
decodeInstr s = let convert = \i -> read (words s !! i) :: Int in (convert 1, convert 3 - 1, convert 5 - 1)

execute :: Instruction -> [Stack] -> (Stack -> Stack -> Stack) -> [Stack]
execute (n, from, to) stacks push = let (movedStack, newStackFrom) = popM n (stacks!!from)
                                        newStackTo = push movedStack (stacks!!to)
                                    in (replaceNth to newStackTo . replaceNth from newStackFrom) stacks

executeM :: [Instruction] -> [Stack] -> (Stack -> Stack -> Stack) -> [Stack]
executeM [] s _ = s
executeM (i:is) s push = executeM is (execute i s push) push

solution1 :: [Instruction] -> [Stack] -> String
solution1 is s = map head (executeM is s pushM)

solution2 :: [Instruction] -> [Stack] -> String
solution2 is s = map head (executeM is s pushMKO)

main = do
        instrStr <- readInstructions
        let instr = map decodeInstr instrStr
        let sol1 = solution1 instr initStacks
        print $ "Solution 1 : " ++ sol1
        let sol2 = solution2 instr initStacks
        print $ "Solution 2 : " ++ sol2
        

