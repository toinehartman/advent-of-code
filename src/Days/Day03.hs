module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Data.Text
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type TreeMap = [Text]

type Coord = (Int, Int)

type Input = TreeMap

type OutputA = Int
type OutputB = Void

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

partA :: Input -> OutputA
partA grid = countTrees start stepper finisher grid
  where
    yLimit = Data.List.length grid
    step = (3, 1)
    start = (0, 0)
    lim = (Data.Text.length $ Data.List.head grid, Data.List.length grid)
    stepper = calcStep step lim
    finisher = (>= yLimit) . snd

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
