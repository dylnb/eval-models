module Tyche.Inference.Enumerate where

import Tyche.Prob
import Tyche.Dist
import Tyche.Model

import qualified Data.Map as Map
import Data.Tuple.Extra (second)
import Control.Monad (forM)
import Data.IORef

enum :: Model a -> IO [(a,LogProb)]
enum m = do x <- runFreeT m
            case x of
              Return r -> return [(r,0)]
              Wrap (Belief dist k) -> fmap concat $ forM (toList dist) $ \(x,lp) ->
                do ds <- enum (k x)
                   return $ [(y,lp+lp2) | (y,lp2) <- ds]
              Wrap (Weight lp k) -> do
                ds <- enum (k ())
                return [(x,lp+lp2) | (x,lp2) <- ds]

enumD :: Ord a => Model a -> IO (Dist a)
enumD m = fromList <$> enum m

enumM :: Ord a => Model a -> IO (Model a)
enumM m = enumD m >>= return . belief . justSupport
  where
    justSupport dist = case dist of
      Discrete assoc -> Discrete $ filter (\(x,p) -> p > 0) assoc
      d              -> d

memoize :: (Ord k, Ord a) => IORef ((Int,Map.Map k (Dist a))) -> (k -> Model a) -> k -> IO (Dist a)
memoize ref f k = do
  (i,cache) <- readIORef ref
  case Map.lookup k cache of
    Just r -> writeIORef ref (i+1,cache) >> return r
    Nothing -> do r <- enumD (f k)
                  writeIORef ref (i,Map.insert k r cache)
                  return r

-- enum :: Model a -> [(a,LogProb)]
-- enum (Pure x) = [(x,0)]
-- enum (Free (Belief dist k)) = do
--   (x,lp) <- toList dist
--   (y,lp2) <- enum (k x)
--   return (y,lp+lp2)
-- enum (Free (Weight lp k)) = do
--   (x,lp2) <- enum (k ())
--   return (x,lp+lp2)

-- enumD :: Ord a => Model a -> Dist a
-- enumD = fromList . enum
