module Tyche.Inference.LikelihoodWeighting where

import Tyche.Model
-- import Tyche.Random
import qualified System.Random.MWC.Probability as MWC
import Tyche.Dist

import Control.Monad (replicateM)

-- Generate a single likelihood weighted sample from the model.
lw :: Model a -> MWC.Prob IO (a,Double)
lw (Pure x) = return (x,0)
lw (Free (Belief dist k)) = do
  x <- sample dist
  lw (k x)
lw (Free (Weight lp k)) = do
  (x,w) <- lw $ k ()
  return (x,w+lp)

-- Take n samples and turn them into a distribution.
lwD :: Ord a => Int -> Model a -> MWC.Prob IO (Dist a)
lwD n model = fromList <$> replicateM n (lw model)
