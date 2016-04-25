module Plotter where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Monad
signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

plotTimes :: [(String,[(Double, Double)])] -> String -> String -> IO ()
plotTimes signals fname title = do
  toFile def fname $ do
    layout_title .= title
    setColors $ concatMap (\x -> [opaque x, opaque x]) [blue, green, red, orange, violet, yellow, black]
    forM_ signals $ \(t,x) -> do
      plot (line t [x])
      plot (points t x)
