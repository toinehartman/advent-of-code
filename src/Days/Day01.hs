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

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = product . sumsTo 2020 . U.sublistsOfSize 2

sumsTo :: (Num a, Eq a) => a -> [[a]] -> [a]
sumsTo n xs = case find (\ys -> sum ys == n) xs of
  Just zs -> zs

------------ PART B ------------
partB :: Input -> OutputB
partB = product . sumsTo 2020 . U.sublistsOfSize 3
