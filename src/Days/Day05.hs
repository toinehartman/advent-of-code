module Days.Day05 (runDay) where

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
import Data.Char (isAlpha)
import Data.Text (unpack)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (unpack <$> Data.Attoparsec.Text.takeWhile isAlpha) `sepBy` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Void

type RowRange = Range

type ColRange = Range

type Range = (Int, Int)

lower :: Range -> Range
lower (min, max) = (min, min + half - 1)
  where
    l = max - min + 1
    half = l `div` 2

upper :: Range -> Range
upper (min, max) = (min + half, max)
  where
    l = max - min + 1
    half = l `div` 2

findSeat :: ColRange -> RowRange -> [Char] -> Int
findSeat c r (d : ds)
  | d == 'F' = findSeat c (lower r) ds
  | d == 'B' = findSeat c (upper r) ds
  | d == 'L' = findSeat (lower c) r ds
  | d == 'R' = findSeat (upper c) r ds
  | otherwise = error "Strange character occurred..."
findSeat (minC, c) (minR, r) ""
  | minC == c && minR == r = c + 8 * r
  | otherwise = error "Range did not converge"
------------ PART A ------------
partA :: Input -> OutputA
partA = maximum . map (findSeat colRange rowRange) . filter (not . null)
  where
    colRange = (0, 7) :: ColRange
    rowRange = (0, 127) :: RowRange

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
