module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
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

type OutputB = Int

data Row

data Col

type Range a = (Int, Int)

lower :: Range a -> Range a
lower (min, max) = (min, min + half - 1)
  where
    l = max - min + 1
    half = l `div` 2

upper :: Range a -> Range a
upper (min, max) = (min + half, max)
  where
    l = max - min + 1
    half = l `div` 2

calcSeatID :: Range Col -> Range Col -> [Char] -> Int
calcSeatID c r (d : ds)
  | d == 'F' = calcSeatID c (lower r) ds
  | d == 'B' = calcSeatID c (upper r) ds
  | d == 'L' = calcSeatID (lower c) r ds
  | d == 'R' = calcSeatID (upper c) r ds
calcSeatID (minC, c) (minR, r) ""
  | minC == c && minR == r = c + 8 * r
  | otherwise = error "Range did not converge"

seatIDs :: Input -> [Int]
seatIDs = map (calcSeatID colRange rowRange) . filter (not . null)
  where
    colRange = (0, 7) :: Range Col
    rowRange = (0, 127) :: Range Row
------------ PART A ------------
partA :: Input -> OutputA
partA = maximum . seatIDs

------------ PART B ------------
findMissingSeat :: [Int] -> Int
findMissingSeat (x1 : x2 : x3 : xs)
  | x2 - 1 == x1 && x2 + 1 == x3 = findMissingSeat (x3 : xs)
  | otherwise = x2 + 1

partB :: Input -> OutputB
partB = findMissingSeat . sort . seatIDs
