module Days.Day02 (runDay) where

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
import Data.Char (isAlpha, isSpace)
import Data.Text (Text, count, index, singleton)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = passwordCombo `sepBy` endOfLine

passwordCombo :: Parser PasswordCombo
passwordCombo = do
  pol <- policy
  skip (== ':')
  skip isSpace
  pass <- password
  return $ PasswordCombo pol pass

password :: Parser Password
password = Data.Attoparsec.Text.takeWhile isAlpha

policy :: Parser Policy
policy = do
  lower <- decimal
  skip (== '-')
  upper <- decimal
  skip isSpace
  c <- anyChar
  return $ Policy lower upper c
------------ TYPES ------------
type Input = [PasswordCombo]

type OutputA = Int

type OutputB = Int

-- Custom types
data PasswordCombo = PasswordCombo Policy Password deriving (Show)

type Password = Text

data Policy = Policy Int Int Char deriving (Show)

isValidA :: PasswordCombo -> Bool
isValidA (PasswordCombo (Policy min max c) pass) = n >= min && n <= max
  where
    n = Data.Text.count (singleton c) pass

count' :: (a -> Bool) -> [a] -> Int
count' p = length . filter p
xor' :: Bool -> Bool -> Bool
xor' True b = not b
xor' False b = b
------------ PART A ------------
partA :: Input -> OutputA
partA = count' isValidA

------------ PART B ------------
isValidB :: PasswordCombo -> Bool
isValidB (PasswordCombo (Policy p1 p2 c) pass) = xor' ((pass `index` (p1 - 1)) == c) ((pass `index` (p2 - 1)) == c)
partB :: Input -> OutputB
partB = count' isValidB
