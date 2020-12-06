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
import Control.Monad (liftM2)
import Data.Functor (($>))
import Data.Text (Text, pack, unpack)
import Util.Parsers (blankLine)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB
------------ PARSER ------------
inputParser :: Parser Input
inputParser = passport `sepBy` blankLine

entry :: Parser Entry
entry = do
  k <- Data.Attoparsec.Text.take 3
  skip (== ':')
  value k

value :: Text -> Parser Entry
value "byr" = do BYr <$> decimal
value "iyr" = do IYr <$> decimal
value "eyr" = do EYr <$> decimal
value "hgt" = do
  d <- decimal
  unit <- takeTill isSpace
  return $ Hgt d (unpack unit)
value "hcl" = do
  c <- takeTill isSpace
  return . HCl $ unpack c
value "ecl" = do
  c <- takeTill isSpace
  return . ECl $ unpack c
value "pid" = do
  id <- takeTill isSpace
  return . PID $ unpack id
value "cid" = do takeTill isSpace $> CID
passport :: Parser Passport
passport = entry `sepBy` space

------------ TYPES ------------
type Passport = [Entry]

data Entry
  = BYr Int
  | IYr Int
  | EYr Int
  | Hgt Int String
  | HCl [Char]
  | ECl String
  | PID String
  | CID
  deriving (Eq, Show)

type Input = [Passport]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
isValidA :: Passport -> Bool
isValidA p
  | l == 7 = CID `notElem` p
  | otherwise = l == 8
  where
    l = length p
partA :: Input -> OutputA
partA = length . filter isValidA

------------ PART B ------------
isValidB :: Passport -> Bool
isValidB = all isValidEntry

isValidEntry :: Entry -> Bool
isValidEntry (BYr y) = y >= 1920 && y <= 2002
isValidEntry (IYr y) = y >= 2010 && y <= 2020
isValidEntry (EYr y) = y >= 2020 && y <= 2030
isValidEntry (Hgt n u)
  | u == "cm" = n >= 150 && n <= 193
  | u == "in" = n >= 59 && n <= 76
isValidEntry (HCl ('#' : c)) = case parseOnly hexadecimal $ pack c of
  Right (_ :: Integer) -> True
  Left (_ :: String) -> False
isValidEntry (ECl s) = s `elem` opts
  where
    opts = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidEntry (PID id)
  | length id == 9 = case parseOnly decimal $ pack id of
    Right (_ :: Integer) -> True
    Left (_ :: String) -> False
isValidEntry CID = True
isValidEntry _ = False
partB :: Input -> OutputB
partB = length . filter (liftM2 (&&) isValidA isValidB)
