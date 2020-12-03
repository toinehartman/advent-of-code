module Program.Time (time) where

import System.CPUTime
-- import System.Environment
import Text.Printf

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 9)
  putStr $ printf " (%0.3f ms)\n" (diff :: Double)
  return v
