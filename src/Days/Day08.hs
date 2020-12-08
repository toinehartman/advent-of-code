module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
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

type OutputB = Void

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving (Show)

type Visited = [Int]

eval :: Int -> Visited -> [Instruction] -> Int
eval i s is
  | visited i s = 0
  | otherwise = case is !! i of
    Nop _ -> eval (i + 1) (visit i s) is
    Acc x -> x + eval (i + 1) (visit i s) is
    Jmp n -> eval (i + n) (visit i s) is

visit :: Int -> Visited -> Visited
visit i vs = xs ++ (y + 1) : ys
  where
    (xs, y : ys) = splitAt i vs

visited :: Int -> Visited -> Bool
visited i is = is !! i == 1
------------ PART A ------------
partA :: Input -> OutputA
partA ins = eval 0 s ins
  where
    s = replicate (length ins) 0

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
