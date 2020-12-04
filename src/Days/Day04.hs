module Days.Day04 (runDay) where

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
import Data.Char (isSpace)
import Data.Text (Text)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

blankLine :: Parser ()
blankLine = endOfLine *> endOfLine
------------ PARSER ------------
inputParser :: Parser Input
inputParser = passport `sepBy` blankLine

key :: Parser Key
key = do
  k <- Data.Attoparsec.Text.take 3
  skip (== ':')
  v <- Data.Attoparsec.Text.takeTill isSpace
  return k

passport :: Parser Passport
passport = key `sepBy` space

------------ TYPES ------------
type Passport = [Key]

type Key = Text

type Input = [Passport]

type OutputA = Int

type OutputB = Void

isValid :: Passport -> Bool
isValid p = length p == 8 || length p == 7 && opt `notElem` p
  where
    opt = "cid"
------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter isValid

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
