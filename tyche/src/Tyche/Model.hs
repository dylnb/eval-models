{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Tyche.Model where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import qualified Data.Map as Map

import Tyche.Prob
import Tyche.Dist

data FreeF f r x = Return r | Wrap (f x)

data FreeT f m r = FreeT { runFreeT :: m (FreeF f r (FreeT f m r)) }

instance (Functor f, Monad m) => Functor (FreeT f m) where
    fmap = liftM

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure  = return
    (<*>) = ap

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return  = FreeT . return . Return
    m >>= f = FreeT $ do
        x <- runFreeT m
        runFreeT $ case x of
            Return r -> f r
            Wrap   w -> wrap $ fmap (>>= f) w

instance MonadTrans (FreeT f) where
    lift = FreeT . liftM Return

-- | Smart constructor for 'Wrap'
wrap :: (Monad m) => f (FreeT f m r) -> FreeT f m r
wrap = FreeT . return . Wrap

-- | Equivalent to @liftF@ from "Control.Monad.Free"
liftF :: (Functor f, Monad m) => f r -> FreeT f m r
liftF x = wrap $ fmap return x

data ModelF next where
  Belief :: Dist a -> (a -> next) -> ModelF next
  Weight :: LogProb -> (() -> next) -> ModelF next

instance Functor ModelF where
  fmap f (Belief dist g) = Belief dist (f . g)
  fmap f (Weight prob g) = Weight prob (f . g)

type Model = FreeT ModelF IO

belief :: Dist a -> Model a
belief dist = liftF $ Belief dist id

weight :: LogProb -> Model ()
weight lp = liftF $ Weight lp id

-- data Free f r = Free (f (Free f r)) | Pure r

-- instance Functor f => Functor (Free f) where
--   fmap f (Pure x) = Pure (f x)
--   fmap f (Free x) = Free ((fmap . fmap) f x)

-- instance Functor f => Applicative (Free f) where
--   pure = Pure
--   (<*>) :: Free f (a -> b) -> Free f a -> Free f b
--   Pure f <*> x = fmap f x
--   Free f <*> x = Free $ fmap (<*> x) f

-- instance Functor f => Monad (Free f) where
--   return = Pure
--   (>>=) :: Free f a -> (a -> Free f b) -> Free f b
--   Pure x >>= f = f x
--   Free x >>= f = Free $ fmap (>>= f) x

-- liftF :: Functor f => f r -> Free f r
-- liftF command = Free (fmap Pure command)

-- data ModelF next where
--   Belief :: Dist a -> (a -> next) -> ModelF next
--   Weight :: LogProb -> (() -> next) -> ModelF next

-- instance Functor ModelF where
--   fmap :: (a -> b) -> ModelF a -> ModelF b
--   fmap f (Belief dist g) = Belief dist (f . g)
--   fmap f (Weight prob g) = Weight prob (f . g)

-- type Model = Free ModelF

-- belief :: Dist a -> Model a
-- belief dist = liftF $ Belief dist id

-- weight :: LogProb -> Model ()
-- weight lp = liftF $ Weight lp id
