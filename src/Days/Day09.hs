module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Maybe (mapMaybe)
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
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

-- findCounterparts :: Int -> Int -> [Int] -> [Int]
-- findCounterparts s x = filter (== s - x)

splice :: Int -> Int -> [Int] -> [Int]
splice x y xs = zs
  where
    (_, ys) = splitAt x xs
    (zs, _) = splitAt (y - x) ys
------------ PART A ------------
-- `checkIfSum2 s xs` returns True if two different elements of xs sum to s
checkIfSum2 :: (Int, [Int]) -> Maybe Int
checkIfSum2 (s, xs) =
  if any ((== s) . sum) $ U.sublistsOfSize 2 xs
    then Nothing
    else Just s

partA :: Input -> OutputA
partA ins = head $ mapMaybe (checkIfSum2 . (\i -> (ins !! (i + n), splice i (i + n) ins))) [0 .. l - n - 1]
  where
    n = 25
    l = length ins

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
