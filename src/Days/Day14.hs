module Days.Day14 (runDay) where

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
import Data.Text (unpack)
import Data.Word (Word64)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  msk <- mask
  endOfLine
  mms <- mem `sepBy` endOfLine
  return (Program msk mms)

mask :: Parser Mask
mask = do
  string "mask = "
  Map.fromList <$> chars

chars :: Parser [(Int, Int)]
chars = do
  cs <- reverse . unpack <$> Data.Attoparsec.Text.take 36
  return $ mapMaybe f (zip [0 ..] cs)
  where
    f :: (Int, Char) -> Maybe (Int, Int)
    f (_, 'X') = Nothing
    f (i, '0') = Just (i, 0)
    f (i, '1') = Just (i, 1)

mem :: Parser Mem
mem = do
  string "mem["
  i <- decimal
  string "] = "
  v :: Word64 <- decimal
  return (Mem i v)

------------ TYPES ------------
type Input = Program

type OutputA = Int

type OutputB = Void

data Program = Program Mask [Mem] deriving (Show)

type Mask = Map Int Int

data Mem = Mem Int Value deriving (Show)

type Value = Word64
------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
