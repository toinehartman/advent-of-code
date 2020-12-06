module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List (foldl')
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
import Util.Parsers (blankLine)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = group `sepBy` blankLine

group :: Parser Group
group = answers `sepBy` endOfLine

answers :: Parser Answers
answers = Set.fromList . unpack <$> Data.Attoparsec.Text.takeWhile1 isAlpha
------------ TYPES ------------
type Input = [Group]

type OutputA = Int

type OutputB = Int

type Group = [Answers]

type Answers = Set Char
------------ PART A ------------
-- partA = id
partA :: Input -> OutputA
partA = sum . map countPerGroup
  where
    countPerGroup :: Group -> Int
    countPerGroup = length . foldl' Set.union Set.empty

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map countPerGroup
  where
    countPerGroup :: Group -> Int
    countPerGroup = length . foldl1 Set.intersection -- fold without base case
