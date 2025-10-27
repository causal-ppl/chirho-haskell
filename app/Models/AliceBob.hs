{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Models.AliceBob (main) where

import Chirho
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Inference.Lazy.MH (mh)
import Control.Monad.Bayes.Sampler.Lazy
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Bayes.Weighted
import Data.Functor.Identity

inferAvg :: WeightedT Sampler Double -> IO Double
inferAvg model = do
  -- IO
  let nSamples = 100000
  samples <- mh 0.4 model
  let prunedSamples = take nSamples samples
  let mean = sum [x * exp (ln d) | (x, d) <- prunedSamples] / fromIntegral nSamples
  return mean

-- Car starting example from Dario -- WebPPL implementation:
{-
    var bob = function() {
      // external variables
      var fuel = flip(0.8)
      var battery = flip(0.9)
      // evolution
      var car_starts = function(fuel, battery) {
        return fuel & battery;
      }
      // condition
      condition(car_starts(fuel,battery) == false);
      // twinning + intervention
      var fuel_int = true;
      var battery_int = battery;
      return car_starts(fuel_int,battery_int);
    }

    viz(Infer(bob))
-}

-- Car starting example from Dario: BOB

-- type MultiVal  a = MultiVal a

getCounterfactual :: MultiVal a -> a
getCounterfactual (MultiVal (ns, f)) = f (constWorld True ns)

run :: Caus m a -> m a
run model = getM model empty

infer model = inferAvg ((\b -> if b then 1.0 else 0.0) <$> model)

-- bobCarModel :: MonadDistribution m
--     => InterventionPointKey m Bool -> Caus m (MultiVal Bool, MultiVal Bool, MultiVal Bool)
-- bobCarModel fuelKey = do -- Caus
--     fuel <- sample (pure (bernoulli 0.8));
--     fuelInt <- new_ fuelKey fuel;
--     battery <- sample (pure (bernoulli 0.9));
--     let carStarts = ((&&) <$> fuelInt) <*> battery;
--     return (fuelInt, battery, carStarts);

bobCarModel ::
  (MonadDistribution m) =>
  InterventionPointKey m Bool ->
  Caus m (MultiVal Bool, MultiVal Bool, MultiVal Bool)
bobCarModel fuelKey = do
  -- Caus
  fuel <- sample (pure (bernoulli 0.8))
  fuelInt <- new_ fuelKey fuel
  battery <- sample (pure (bernoulli 0.9))
  let carStarts = ((&&) <$> fuelInt) <*> battery
  return (fuelInt, battery, carStarts)

bobInferenceCode :: IO ()
bobInferenceCode = do
  -- IO
  -- Generate the intervention point key
  fuelKey <- createKey
  let intervenedConditionedModel = do
        -- Caus
        -- Intervene and create branching point
        (_, _, carStarts) <- do_ fuelKey (Value True) "fuelTrue" (bobCarModel fuelKey)
        -- Condition on the factual value of carStarts
        conditionFactual not carStarts
        -- Retrieve the counterfactual value of carStarts
        return $ getCounterfactual carStarts
  -- Run inference
  avg <- infer $ run intervenedConditionedModel
  print avg

bobCarModelIntervenedAndConditioned ::
  (MonadMeasure m) =>
  InterventionPointKey m Bool ->
  Caus m (MultiVal Bool, MultiVal Bool, MultiVal Bool)
bobCarModelIntervenedAndConditioned fuelKey = do
  -- Caus
  let intervenedModel = do_ fuelKey (Value True) "fuelTrue" (bobCarModel fuelKey)
  (fuelInt, battery, carStarts) <- intervenedModel
  conditionFactual not carStarts
  return (fuelInt, battery, carStarts)

bobCarModelCF :: (MonadMeasure m) => IO (m Bool)
bobCarModelCF = do
  -- IO
  fuelKey <- createKey
  let model = bobCarModelIntervenedAndConditioned fuelKey
  let distModel = getM model empty -- run the model in the empty list, getting out a sampling distribution
  -- transform the model to only output the counterfactual values
  let distCFCarStarts = do
        -- Caus
        (_, _, carStarts) <- distModel
        let (ns, val) = getMultiVal carStarts
        return (val (constWorld True ns))
  return distCFCarStarts

bobCarStartsMain :: IO ()
bobCarStartsMain = do
  model <- bobCarModelCF
  avg <- inferAvg ((\b -> if b then 1.0 else 0.0) <$> model)
  print avg

-- Dario's example: ALICE

{-
    var alice = function() {
      // external variables
      var fuel = flip(0.8)
      // evolution
      var car_starts = function(fuel) {
        var electronics = flip(0.9)
        return fuel & electronics;
      }
      // condition
      condition(car_starts(fuel) == false);
      // twinning + intervention
      var fuel_int = true;
      return car_starts(fuel_int);
    }
    viz(Infer(alice))
-}

aliceCarModel ::
  (MonadDistribution m) =>
  InterventionPointKey m Bool ->
  Caus m (MultiVal Bool, MultiVal Bool)
aliceCarModel fuelKey = do
  -- Caus
  fuel <- sample (pure (bernoulli 0.8))
  fuelInt <- new_ fuelKey fuel
  let electronicNoise f = do
        -- Caus
        electronics <- bernoulli 0.9
        return $ f && electronics
  carStarts <- sample (electronicNoise <$> fuelInt)
  return (fuelInt, carStarts)

aliceCarModelIntervenedAndConditioned ::
  (MonadMeasure m) =>
  InterventionPointKey m Bool ->
  Caus m (MultiVal Bool, MultiVal Bool)
aliceCarModelIntervenedAndConditioned fuelKey = do
  -- Caus
  let intervenedModel = do_ fuelKey (Value True) "fuelTrue" (aliceCarModel fuelKey)
  (fuelInt, carStarts) <- intervenedModel
  conditionFactual not carStarts
  return (fuelInt, carStarts)

aliceCarModelCF :: (MonadMeasure m) => IO (m Bool)
aliceCarModelCF = do
  -- IO
  fuelKey <- createKey
  let model = aliceCarModelIntervenedAndConditioned fuelKey
  let distModel = getM model empty
  let distCFCarStarts = do
        -- Caus
        (_, carStarts) <- distModel
        let (ns, val) = getMultiVal carStarts
        return (val (constWorld True ns))
  return distCFCarStarts

aliceCarStartsMain :: IO ()
aliceCarStartsMain = do
  model <- aliceCarModelCF
  avg <- inferAvg ((\b -> if b then 1.0 else 0.0) <$> model)
  print avg

-- Dario's question on expressiveness losses if we identify keys with names

modelOperation1 :: Caus m (MultiVal a) -> InterventionPointKey m Int -> Caus m (MultiVal a)
modelOperation1 model key =
  let newModel1 = do_ key (Value 5) "branch1" model
   in let newModel2 = do_ key (Value 10) "branch2" newModel1
       in newModel2

modelOperation2 :: Caus m (MultiVal a) -> InterventionPointKey m Int -> InterventionPointKey m Int -> Caus m (MultiVal a)
modelOperation2 model key1 key2 =
  let newModel1 = do_ key1 (Value 5) "branch" model
   in let newModel2 = do_ key2 (Value 10) "branch" newModel1
       in newModel2

main_models_1 :: IO ()
main_models_1 = do
  key <- createKey
  let model :: Caus Identity (MultiVal Int) = new_ key (pure 1)
  let model2 = modelOperation1 model key
  let runModel2 = runIdentity $ getM model2 empty
  print runModel2

main_models_2 :: IO ()
main_models_2 = do
  key1 <- createKey
  key2 <- createKey
  let model :: Caus Identity (MultiVal (Int, Int)) = do
        a <- new_ key1 (pure 1)
        b <- new_ key2 (pure 1)
        return $ (,) <$> a <*> b
  let model2 = modelOperation2 model key1 key2
  let runModel2 = runIdentity $ getM model2 empty
  print runModel2

-- Main function

main :: IO ()
main = do
  aliceCarStartsMain
  bobCarStartsMain
  main_models_1
  main_models_2
