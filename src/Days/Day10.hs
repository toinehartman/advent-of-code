module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
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

type OutputB = Void

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
partB :: Input -> OutputB
partB = error "Not implemented yet!"
