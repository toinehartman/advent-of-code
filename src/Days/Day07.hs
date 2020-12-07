module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set, union)
import qualified Data.Set as Set

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Data.Char (isPunctuation)
import Data.Functor (($>))
import Data.Text (unpack)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = bag `sepBy` endOfLine <?> "InputParser"

bag :: Parser Bag
bag = do
  col <- color
  string " bags contain "
  cts <- Map.fromList <$> contents
  skip (== '.')
  return (Bag col cts) <?> "BagParser"

color :: Parser Color
color = unpack <$> scan 0 p <?> "ColorParser"
  where
    p 0 ' ' = Just 1
    p 1 ' ' = Nothing -- stop at the second space
    p n _ = Just n

contents :: Parser [(Color, Int)]
contents =
  choice
    [ string "no other bags" $> [],
      bagCount `sepBy1` string ", "
    ]
    <?> "ContentsParser"

bagCount :: Parser (Color, Int)
bagCount = do
  c <- decimal
  skipSpace
  col <- color
  skipWhile $ not . isPunctuation
  return (col, c) <?> "BagCountParser"
------------ TYPES ------------
type Input = [Bag]

type OutputA = Int

type OutputB = Void

type Color = String

type Contents = Map Color Int

data Bag = Bag Color Contents deriving (Show)

------------ PART A -----------
containerMap :: [Bag] -> Map Color [Color]
containerMap = foldl' f Map.empty
  where
    f m (Bag s mb)
      | Map.null mb = m
      | otherwise = Map.unionWith (++) m (Map.fromList . map (,[s]) $ Map.keys mb)

bfs :: [Color] -> Map Color [Color] -> Set Color
bfs [] _ = Set.empty
bfs (x : xs) m = case l of
  Just next -> Set.fromList next `union` bfs (xs ++ next) m
  Nothing -> bfs xs m
  where
    l = x `Map.lookup` m

-- partA = containerMap
partA :: Input -> OutputA
partA = length . bfs bag . containerMap
  where
    bag = ["shiny gold"]

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
