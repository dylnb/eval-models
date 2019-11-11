module Tyche.Random where

import Tyche.Prob (Prob)

import System.Random (StdGen, mkStdGen, random)
import Control.Monad.Trans.State.Lazy (State, state, evalState)
import qualified System.Random.MWC.Probability as MWC

type Rand a = State StdGen a

runRand :: Int -> Rand a -> a
runRand seed r = evalState r (mkStdGen seed)

-- Sample uniformly from [0,1)
rand :: Rand Double
rand = state random

choose :: Prob -> Rand a -> Rand a -> Rand a
choose p x y = do
  r <- rand
  if r < p then x else y

runSampler dist = MWC.createSystemRandom >>= MWC.sample dist
