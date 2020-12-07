module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
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
countCommonAnswers :: (Answers -> Answers -> Answers) -> Group -> Int
countCommonAnswers f = length . foldl1 f

partA :: Input -> OutputA
partA = sum . map (countCommonAnswers Set.union)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (countCommonAnswers Set.intersection)
