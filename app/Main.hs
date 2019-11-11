module Main where

import Tyche.Model
import Tyche.Prelude
import Tyche.Dist
import Tyche.Random
import Tyche.Inference.Enumerate
-- import Tyche.Inference.LikelihoodWeighting
-- import Tyche.Inference.ParticleCascade
import Utils

import Control.Monad (when)

model :: Model (Bool, Bool)
model = do
  x <- toss 0.4
  y <- toss 0.6
  when (x || y) $ weight (-1)
  return (x,y)

main :: IO ()
main = do
  -- (\(Discrete d) -> showEnums d) $ enumD model
  putStrLn "-----"
  -- (\(Discrete d) -> showEnums d) =<< runSampler (lwD 1000 model)
  -- print $ create >>= MWC.sample (pcD 1000 model)
