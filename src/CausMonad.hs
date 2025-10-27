{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module CausMonad where
import Data.HMap
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class
import GHC.List
import Common
import Val
import Prelude hiding (foldl)

newtype Caus m a = M (HMap -> m a)

getM :: Caus m a -> HMap -> m a
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

type Delta m a = List (Intervention m a, Name)

new_ :: (Monad m) => HKey x (Delta m a) -> Val a -> Caus m (Val a)
new_ key defaultValue =
    M
    ( \interventions ->
        let ys = findWithDefault [] key interventions
         in foldl (\comp (x, name) -> comp >>= \val -> intervene val x name) (pure defaultValue) ys
    )

do_ :: HKey x (Delta m a) -> Intervention m a -> Name -> Caus m b -> Caus m b
do_ key intervention name (M f) =
  M
    ( \oldMap ->
        let xs = findWithDefault [] key oldMap
         in let newXs = xs ++ [(intervention, name)]
             in let newMap = insert key newXs oldMap
                 in f newMap
    )
