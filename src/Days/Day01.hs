module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
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

------------ PART A ------------
partA :: Input -> OutputA
partA nums = case find (\(x, y) -> x + y == 2020) $ createPairs nums of
  Just (n, m) -> n * m
  _ -> error "No pair sums to 2020"

createPairs :: [a] -> [(a, a)]
createPairs [] = []
createPairs (x : xs) = map ((,) x) xs ++ createPairs xs

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
