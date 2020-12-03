module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Data.Text (Text, index, length)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type TreeMap = [Text]

type Coord = (Int, Int)

type Input = TreeMap

type OutputA = Int
type OutputB = Int

------------ PARSER ------------
isMapSymbol :: Char -> Bool
isMapSymbol c = (c == '.') || (c == '#')

inputParser :: Parser Input
inputParser = takeWhile1 isMapSymbol `sepBy` endOfLine

------------ PART A ------------
loc :: Coord -> TreeMap -> Char
loc (x, y) grid = row `index` x
  where
    row = grid !! y

countTree :: Char -> Int
countTree '#' = 1
countTree '.' = 0

calcStep :: Coord -> Coord -> Coord -> Coord
calcStep (dx, dy) (limx, _) (x, y) = ((x + dx) `mod` limx, y + dy)

countTrees :: Coord -> (Coord -> Coord) -> (Coord -> Bool) -> TreeMap -> Int
countTrees c stepper p grid
  | finished = 0
  | otherwise = countTree (loc c grid) + countTrees (stepper c) stepper p grid
  where
    finished = p c

countForStep :: Coord -> TreeMap -> Int
countForStep step grid = countTrees start stepper finisher grid
  where
    yLimit = Data.List.length grid
    start = (0, 0)
    lim = (Data.Text.length $ Data.List.head grid, Data.List.length grid)
    stepper = calcStep step lim
    finisher = (>= yLimit) . snd

partA :: Input -> OutputA
partA = countForStep step
  where
    step = (3, 1)
------------ PART B ------------
partB :: Input -> OutputB
partB grid = product $ map (`countForStep` grid) steps
  where
    steps = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
