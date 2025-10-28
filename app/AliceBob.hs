{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module AliceBob (main) where

import Chirho
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Inference.Lazy.MH (mh)
import Control.Monad.Bayes.Sampler.Lazy
import Control.Monad.Bayes.Weighted

-- Inference utilities
inferAvg :: WeightedT Sampler Double -> IO Double
inferAvg model = do
  -- IO
  let nSamples = 100000
  samples <- mh 0.4 model
  let prunedSamples = take nSamples samples
  let mean = sum [x * exp (ln d) | (x, d) <- prunedSamples] / fromIntegral nSamples
  return mean

infer model = inferAvg ((\b -> if b then 1.0 else 0.0) <$> model)

-- Car starting example: Bob

-- WebPPL implementation:
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

-- ChiRho implementation:

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


-- Car starting example: Alice

-- WebPPL implementation:
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

-- ChiRho implementation:
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

aliceInferenceCode :: IO ()
aliceInferenceCode = do
  -- IO
  -- Generate the intervention point key
  fuelKey <- createKey
  let intervenedConditionedModel = do
        -- Caus
        -- Intervene and create branching point
        (_, carStarts) <- do_ fuelKey (Value True) "fuelTrue" (aliceCarModel fuelKey)
        -- Condition on the factual value of carStarts
        conditionFactual not carStarts
        -- Retrieve the counterfactual value of carStarts
        return $ getCounterfactual carStarts
  -- Run inference
  avg <- infer $ run intervenedConditionedModel
  print avg

-- Main function

main :: IO ()
main = do
  print "Alice's car starting probability if she had refueled her car, given that her car did not start:"
  aliceInferenceCode
  print "Bob's car starting probability if he had refueled his car, given that his car did not start:"
  bobInferenceCode
