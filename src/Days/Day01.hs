module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
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
sumsTo n = head . filter (\ys -> sum ys == n)

------------ PART B ------------
partB :: Input -> OutputB
partB = product . sumsTo 2020 . U.sublistsOfSize 3
