module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Either (rights)
import Data.List (findIndices)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (instruction `sepBy` endOfLine) <* endOfInput

instruction :: Parser Instruction
instruction = choice [jump, nop, acc]

jump :: Parser Instruction
jump = do
  string "jmp "
  x <- signed decimal
  return $ Jmp x

nop :: Parser Instruction
nop = do
  string "nop "
  x <- signed decimal
  return $ Nop x

acc :: Parser Instruction
acc = do
  string "acc "
  x <- signed decimal
  return $ Acc x
------------ TYPES ------------
type Input = [Instruction]

type OutputA = Int

type OutputB = Int

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving (Show)

type Visited = [Int]

eval :: Int -> Visited -> [Instruction] -> Either Int Int
eval i s is
  | i >= length is = Right 0 -- End of instruction list reached
  | visited i s = Left 0 -- Loop
  | otherwise = case is !! i of
    Nop _ -> eval (i + 1) (visit i s) is
    Acc x -> x `add` eval (i + 1) (visit i s) is
    Jmp n -> eval (i + n) (visit i s) is
  where
    add :: Int -> Either Int Int -> Either Int Int
    add x (Left y) = Left $ x + y
    add x (Right y) = Right $ x + y

visit :: Int -> Visited -> Visited
visit i vs = xs ++ (y + 1) : ys
  where
    (xs, y : ys) = splitAt i vs

visited :: Int -> Visited -> Bool
visited i is = is !! i == 1
------------ PART A ------------
partA :: Input -> OutputA
partA ins = case eval 0 s ins of
  Left x -> x
  Right _ -> error "Should not have reached end of instruction list"
  where
    s = replicate (length ins) 0

------------ PART B ------------
replace :: [Instruction] -> Int -> [Instruction]
replace ins i = xs ++ swap y : ys
  where
    (xs, y : ys) = splitAt i ins
    swap :: Instruction -> Instruction
    swap (Nop x) = Jmp x
    swap (Jmp x) = Nop x

findSwappables :: [Instruction] -> [Int]
findSwappables = findIndices swappable
  where
    swappable (Nop _) = True
    swappable (Jmp _) = True
    swappable _ = False

------
partB :: Input -> OutputB
partB ins = head . rights . map (eval i s . replace ins) $ findSwappables ins
  where
    i = 0
    s = replicate (length ins) 0
