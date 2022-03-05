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
import Data.Bits (Bits (setBit))
import Foreign (Bits (clearBit))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (programParser `sepBy` endOfLine) <* endOfInput

programParser :: Parser Program
programParser = do
  msk <- maskParser
  endOfLine
  mms <- Map.fromList <$> memParser `sepBy` endOfLine
  return (Program msk mms)

maskParser :: Parser Mask
maskParser = do
  string "mask = "
  Map.fromList <$> charsParser

charsParser :: Parser [(Int, Int)]
charsParser = do
  cs <- reverse . unpack <$> Data.Attoparsec.Text.take 36
  return $ mapMaybe f (zip [0 ..] cs)
  where
    f :: (Int, Char) -> Maybe (Int, Int)
    f (_, 'X') = Nothing
    f (i, '0') = Just (i, 0)
    f (i, '1') = Just (i, 1)

memParser :: Parser (Int, Value)
memParser = do
  string "mem["
  i <- decimal
  string "] = "
  v :: Word64 <- decimal
  return (i, v)

------------ TYPES ------------
type Input = [Program]

type OutputA = Word64

type OutputB = Void

data Program = Program Mask Mem deriving (Show)

type Mask = Map Int Int

type Mem = Map Int Value

type Value = Word64
------------ PART A ------------
masked :: Value -> Mask -> Value
masked v msk = Map.foldlWithKey' aux v msk
  where
    aux :: Value -> Int -> Int -> Value
    aux v i 0 = clearBit v i
    aux v i 1 = setBit v i
maskProgram :: Program -> Mem
maskProgram (Program msk mem) = Map.map (`masked` msk) mem

combineMems :: [Mem] -> Mem
combineMems = foldr1 Map.union
partA :: Input -> OutputA
partA = sum . Map.elems . combineMems . map maskProgram

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
