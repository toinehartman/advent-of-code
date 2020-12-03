module Program.RunDay (runDay) where

import Control.Exception (SomeException, catch)
import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Text (pack)
import System.Directory (doesFileExist)
import Program.Time (time)

runDay :: (Show a, Show b, Show i) => Parser i -> (i -> a) -> (i -> b) -> Bool -> String -> IO ()
runDay inputParser partA partB verbose inputFile = do
  input <- runExceptT $ do
    inputFileExists <- liftIO $ doesFileExist inputFile
    fileContents <-
      if inputFileExists
        then liftIO $ readFile inputFile
        else throwError $ "I couldn't read the input! I was expecting it to be at " ++ inputFile
    case parseOnly inputParser . pack $ fileContents of
      Left e -> throwError $ "Parser failed to read input. Error " ++ e
      Right i -> do
        when verbose $ do
          liftIO $ putStrLn "Parser output:"
          liftIO . print $ i
        return i
  processInput input
  where
    processInput (Left x) = putStrLn x
    processInput (Right i) = do
      putStr "Part A:"
      let compA = partA i
      catch
        ( do
            res <- time $ compA `seq` return compA
            print res
        )
        (\m -> return (m :: SomeException) >> putStrLn "Couldn't run Part A!" >> when verbose (print m))
      putStr "Part B:"
      let compB = partB i
      catch
        ( do
            res <- time $ compB `seq` return compB
            print res
        )
        (\m -> return (m :: SomeException) >> putStrLn "Couldn't run Part B!" >> when verbose (print m))
