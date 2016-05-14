{-# LANGUAGE TupleSections #-}
import Plotter
import Timer
import Set
import Control.DeepSeq
import System.Random

makeSet n = foldl push empty (take n $ randoms (mkStdGen 8))

pushesAlreadyIn set = push set 1
pushesNotIn set = push set 0
containsIn set = contains set 1
containsInLong set = contains set 50
containsNotIn set = contains set 0

testAndTime f (x,s) = (realToFrac x,).(*10000.0).realToFrac <$> (timeit $ f s)

main = do
  let scale = 1000
  let sets = map (\x -> (x*scale, makeSet (x*scale))) [0..10::Int] :: [(Int, BTree Int)]
  print $ height $ snd $ head $ tail sets
  print $ size $ snd $ head $ tail sets
  putStr $ deepseq sets ""
  pais <- mapM (testAndTime pushesAlreadyIn) sets
  pnis <- mapM (testAndTime pushesNotIn) sets
  cis <- mapM (testAndTime containsIn) sets
  cils <- mapM (testAndTime containsInLong) sets
  cnis <- mapM (testAndTime containsNotIn) sets
  sizes <- mapM (testAndTime size) sets
  pops <- mapM (testAndTime pop) sets

  plotTimes [ ("Pushes (Already in)",pais)
            , ("Pushes (Not In)",pnis)
            , ("Contains (in)",cis)
            , ("Contains (long)",cils)
            , ("Contains (not in)",cnis)
            , ("Sizes",sizes)
            , ("Pops",pops)
            ] "comparison.png" "comparison"
