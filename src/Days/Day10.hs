module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap, (!))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

countJumps :: [Int] -> IntMap Int
countJumps [_] = IntMap.fromAscList [(1, 0), (2, 0), (3, 1)]
countJumps (x1 : x2 : xs) = IntMap.adjust (+ 1) diff next
  where
    diff = x2 - x1
    next = countJumps (x2 : xs)
------------ PART A ------------
partA :: Input -> OutputA
partA ins = (counts ! 1) * (counts ! 3)
  where
    counts = countJumps . sort $ 0 : ins

------------ PART B ------------
countArrangements :: Int -> [Int] -> Int
countArrangements jump [x1, _, x3]
  | x1 + jump >= x3 = 2
  | otherwise = 1
countArrangements jump (x1 : x2 : x3 : x4 : xs)
  | x1 + jump >= x4 = 4 * countArrangements jump (x4 : xs)
  | x1 + jump >= x3 = 2 * countArrangements jump (x3 : x4 : xs)
  | x1 + jump >= x2 = countArrangements jump (x2 : x3 : x4 : xs)
  | otherwise = error "WTF?"
countArrangements _ _ = 1

-- countArrangements jump (x1 : x2 : x3 : x4 : xs)
--   | x1 + jump == x2 = countArrangements jump (x2 : x3 : x4 : xs)
--   | x1 + jump <= x3 = 1 + countArrangements jump (x3 : x4 : xs)
--   | x1 + jump <= x4 = 3 + countArrangements jump (x4 : xs)
--   | otherwise = error "WTF?"
partB :: Input -> OutputB
partB = countArrangements 3 . sort . (0 :)
