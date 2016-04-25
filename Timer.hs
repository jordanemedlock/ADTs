module Timer where

import Data.Time.Clock


timeit x = do
  start <- getCurrentTime
  putStr $ seq x ""
  end <- getCurrentTime
  return $ diffUTCTime end start
