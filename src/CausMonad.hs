{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module CausMonad where
import Data.HMap 
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class
import GHC.List
import Common
import MultiVal
import Prelude hiding (foldl)

type Delta m a = List (Intervention m a, Name)

type InterventionPointKey m a = HKey T (Delta m a)

type InterventionEnv = HMap 

newtype Caus m a = M (InterventionEnv  -> m a)

getM :: Caus m a -> InterventionEnv  -> m a
getM (M v) = v

instance MonadTrans Caus where
  lift :: (Monad m) => m a -> Caus m a
  lift x = M (const x)

instance (Functor m) => Functor (Caus m) where
  fmap :: (a -> b) -> Caus m a -> Caus m b
  fmap f (M g) = M (fmap f . g)

instance (Applicative m) => (Applicative (Caus m)) where
  pure :: a -> Caus m a
  pure x = M (\_ -> pure x)
  (<*>) :: Caus m (a -> b) -> Caus m a -> Caus m b
  (<*>) (M f) (M g) = M (\hmap -> f hmap <*> g hmap)

instance (Monad m) => Monad (Caus m) where
  -- Monadic structure given literally as that of a reader monad.
  (>>=) :: Caus m a -> (a -> Caus m b) -> Caus m b
  M v >>= f =
    M
      ( \hmap -> do
          x <- v hmap
          let M v' = f x
          v' hmap
      )

instance (MonadDistribution m) => MonadDistribution (Caus m) where
  random :: (MonadDistribution m) => Caus m Double
  random = lift random

instance (MonadFactor m) => MonadFactor (Caus m) where
  score :: MonadFactor m => Log Double -> Caus m ()
  score v = lift $ score v

instance (MonadMeasure m) => MonadMeasure (Caus m)


new_ :: (Monad m) => InterventionPointKey m a -> MultiVal a -> Caus m (MultiVal a)
new_ key defaultVal =
    M
    ( \interventions ->
        let ys = findWithDefault [] key interventions
         in foldl (\comp (x, name) -> comp >>= \val -> intervene val x name) (pure defaultVal) ys
    )

do_ :: InterventionPointKey m a -> Intervention m a -> Name -> Caus m b -> Caus m b
do_ key intervention name (M f) =
  M
    ( \oldMap ->
        let xs = findWithDefault [] key oldMap
         in let newXs = xs ++ [(intervention, name)]
             in let newMap = insert key newXs oldMap
                 in f newMap
    )
