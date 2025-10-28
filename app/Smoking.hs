{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smoking (main) where

import Chirho
import Control.Monad
import Control.Monad.Bayes.Class hiding (histogram)
import Control.Monad.Bayes.Inference.Lazy.MH (mh)
import Control.Monad.Bayes.Sampler.Lazy
import Control.Monad.Bayes.Sampler.Strict (sampleIO, sampler)
import Control.Monad.Bayes.Weighted
import Control.Monad.Trans.Class (lift)
import Graphics.Matplotlib
import Control.Monad.Bayes.Enumerator (toEmpiricalWeighted, toEmpirical)
import Control.Monad.Bayes.Inference.MCMC
import Control.Monad.Bayes.Inference.TUI (MCMCData(samples))

-- import Graphics.Matplotlib (plot)

{- MonadBayes helpers -}

inferAvg :: Int -> WeightedT Sampler Double -> IO Double
inferAvg nSamples model = do
  -- IO
  -- let nSamples = 10000;
  samples <- mh 0.4 model
  let prunedSamples = take nSamples samples
  let mean = sum [x * exp (ln d) | (x, d) <- prunedSamples] / fromIntegral nSamples
  return mean

{-
Causal probabilistic programming without tears
    https://basisresearch.github.io/chirho/tutorial_i.html

I) Write model with fixed parameters (distributions/kernels) -> Forward execution.
   Manually intervene -> Forward execution

II) Use `do` to intervene automatically -> Forward execution (same as I)

III) Put bayesian prior over parameters
     -> Condition on some observations
     -> Infer parameters (causal inference)

IV*) Put bayesian prior over models (causal structures)
     -> Infer which causal structures we use
-}

{- Observation 1: causal models are probabilistic programs -}

newtype Stress = Stress Bool deriving (Eq, Show)

newtype Smoke = Smoke Bool deriving (Eq, Show)

newtype Cancer = Cancer Bool deriving (Eq, Show)

-- average treatment effect
ate :: [(Stress, Smoke, Cancer)] -> Double
ate sample = (fromIntegral cancer_and_smokes) / (fromIntegral smokes) - (fromIntegral cancer_and_not_smokes) / (fromIntegral not_smokes)
  where
    cancer_and_smokes = 1 + length [() | (Stress stress, Smoke smoke, Cancer cancer) <- sample, smoke == True && cancer == True]
    smokes = 1 + length [() | (Stress stress, Smoke smoke, Cancer cancer) <- sample, smoke == True]
    cancer_and_not_smokes = 1 + length [() | (Stress stress, Smoke smoke, Cancer cancer) <- sample, smoke == False && cancer == True]
    not_smokes = 1 + length [() | (Stress stress, Smoke smoke, Cancer cancer) <- sample, smoke == False]

-- a parameterized statistical model

type Param m = (m Stress, Stress -> m Smoke, Stress -> Smoke -> m Cancer)

stat_model ::
  (MonadDistribution m) =>
  Param m ->
  m (Stress, Smoke, Cancer)
stat_model (stress_prob, smokes_cond_prob, cancer_cond_prob) = do
  stress <- stress_prob
  smoke <- smokes_cond_prob stress
  cancer <- cancer_cond_prob stress smoke
  return (stress, smoke, cancer)

-- some hand chosen example parameters
default_params :: (MonadDistribution m) => Param m
default_params = (stress_prob, smokes_cond_prob, cancer_cond_prob)
  where
    stress_prob = Stress <$> bernoulli 0.5

    smokes_cond_prob (Stress True) = Smoke <$> bernoulli 0.8
    smokes_cond_prob (Stress False) = Smoke <$> bernoulli 0.2

    cancer_cond_prob (Stress True) (Smoke True) = Cancer <$> bernoulli 0.85
    cancer_cond_prob (Stress True) (Smoke False) = Cancer <$> bernoulli 0.8
    cancer_cond_prob (Stress False) (Smoke True) = Cancer <$> bernoulli 0.15
    cancer_cond_prob (Stress False) (Smoke False) = Cancer <$> bernoulli 0.1

-- Alternative params where this time the effect is not confounded with stress for part VII
data_params :: (MonadDistribution m) => Param m
data_params = (stress_prob, smokes_cond_prob, cancer_cond_prob)
  where
    stress_prob = Stress <$> bernoulli 0.5

    smokes_cond_prob (Stress True) = Smoke <$> bernoulli 0.6
    smokes_cond_prob (Stress False) = Smoke <$> bernoulli 0.3

    cancer_cond_prob (Stress True) (Smoke True) = Cancer <$> bernoulli 0.9
    cancer_cond_prob (Stress True) (Smoke False) = Cancer <$> bernoulli 0.1
    cancer_cond_prob (Stress False) (Smoke True) = Cancer <$> bernoulli 0.3
    cancer_cond_prob (Stress False) (Smoke False) = Cancer <$> bernoulli 0.1


{- Simulating Observational Data with Fixed Parameters -}

visualise_smoking_effects :: Int -> [Double] -> [Double] -> [Char] -> Matplotlib
visualise_smoking_effects nrBins smokeTrues smokeFalses titleString =
  mp
    % histogram smokeTrues nrBins @@ [o2 "density" True, o2 "histtype" "stepfilled", o2 "alpha" (0.8 :: Double), o2 "label" "Smoking"]
    % histogram smokeFalses nrBins @@ [o2 "density" True, o2 "histtype" "stepfilled", o2 "alpha" (0.8 :: Double), o2 "label" "Not Smoking"]
    % xlabel "Probability of Cancer given Smoking Status"
    % ylabel "Density"
    % legend
    % title titleString

visualise_ate :: Int -> [Double] -> String -> Matplotlib
visualise_ate nrBins ateEstimates titleString =
  mp
    % histogram ateEstimates nrBins @@ [o2 "density" True, o2 "histtype" "stepfilled", o2 "alpha" (0.8 :: Double), o2 "label" "ATE Estimates"]
    % xlabel "Average Treatment Effect (ATE)"
    % ylabel "Density"
    % legend
    % title titleString

main_I :: IO ()
main_I = do
  -- To be more pedantic this is not quite what ChiRho does.
  -- we have approx of distr, they have distr of approx's
  print "=== I- Observational Model ==="
  let nSamples = 10000
  let model smoke_cond = do
        -- MonadInfer
        (_, smoke, cancer) <- stat_model default_params
        condition (smoke == smoke_cond)
        return (if cancer == Cancer True then 1.0 else 0.0)
  smoke_true_avg <- inferAvg nSamples (model $ Smoke True)
  print "Estimated P(cancer | smoking): "
  print smoke_true_avg
  smoke_false_avg <- inferAvg nSamples (model $ Smoke False)
  print "Estimated P(cancer | not smoking): "
  print smoke_false_avg
  -- Repeat experiments for plotting
  let filename = "I_smoking_effects.png"
  print ("Plotting Smoking Effects and saving in " ++ filename)
  let nExperiments = 500
  results <- replicateM nExperiments $ do
    -- print "Sampling for plot..."
    smoke_true <- inferAvg nSamples (model $ Smoke True)
    smoke_false <- inferAvg nSamples (model $ Smoke False)
    return (smoke_true, smoke_false)
  let (smoke_trues, smoke_falses) = unzip results
  -- Matplotlib make plot
  result <- file ("output/" ++ filename) $ visualise_smoking_effects 10 smoke_trues smoke_falses ("Smoking Effects on Cancer Probability" ++ " (n=" ++ show nExperiments ++ ")")
  print result

{- Applying an intervention -}

intervened_stat_model ::
  (MonadDistribution m) =>
  Param m ->
  Smoke ->
  m (Stress, Smoke, Cancer)
intervened_stat_model (stress_prob, smokes_cond_prob, cancer_cond_prob) smoke_int = do
  stress <- stress_prob
  smoke <- return smoke_int
  cancer <- cancer_cond_prob stress smoke
  return (stress, smoke, cancer)

main_II :: IO ()
main_II = do
  print "=== II- Manually Intervened Model ==="
  -- Directly estimate the average using MonadBayes inference.
  let model intv = do
        -- MonadInfer
        -- (_, _, cancer) <- intervened_stat_model data_params intv
        (_, _, cancer) <- intervened_stat_model default_params intv
        return (if cancer == Cancer True then 1.0 else 0.0)
  let nSamples = 10000
  true_avg <- inferAvg nSamples (model (Smoke True))
  false_avg <- inferAvg nSamples (model (Smoke False))
  -- Mathematically correct value: 0.5
  print "P(cancer | do(smoking)) [manually]: "
  print true_avg
  -- Mathematically correct value: 0.45
  print "P(cancer | do(not smoking)) [manually]: "
  print false_avg
  -- Repeat experiments for plotting
  let filename = "II_smoking_effects_intervened.png"
  print ("Plotting Smoking Effects under Intervention and saving in " ++ filename)
  let nExperiments = 500
  results <- replicateM nExperiments $ do
    smoke_true <- inferAvg nSamples (model (Smoke True))
    smoke_false <- inferAvg nSamples (model (Smoke False))
    return (smoke_true, smoke_false)
  let (smoke_trues, smoke_falses) = unzip results
  -- Matplotlib make plot
  pltResult1 <- file ("output/" ++ filename) $ visualise_smoking_effects 10 smoke_trues smoke_falses ("Smoking Effects on Cancer Probability under Intervention" ++ " (n=" ++ show nExperiments ++ ")")
  print pltResult1
  -- Compute ATE which is E[Phat(cancer | do(smoking))] - E[Phat(cancer | do(not smoking))]
  -- Mathematically correct value: ATE = 0.05
  let ate_estimate = (-) <$> smoke_trues <*> smoke_falses
  print "Estimated ATE (using manual intervention) saved in output/II_ate_estimate.png"
  pltResult2 <- file "output/II_ate_estimate.png" $ visualise_ate 10 ate_estimate "Estimated ATE Distribution over Population"

  print pltResult2

{- Transforming Causal Models using ChiRho -}

causal_model ::
  (MonadDistribution m) =>
  Param m ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal (Stress, Smoke, Cancer))
causal_model (stress_prob, smokes_cond_prob, cancer_cond_prob {--}) smoke_key = do
  -- Caus
  stress <- lift $ sample (pure stress_prob)
  smoke <- lift $ sample (smokes_cond_prob <$> stress)
  smokeInt <- new_ smoke_key smoke
  cancer <- lift $ sample (cancer_cond_prob <$> stress <*> smokeInt)
  return $ (,,) <$> stress <*> smokeInt <*> cancer -- [Theo] Not sure if broadcasting here breaks sth. At least make sure not to postcompose this with anything, because this WILL break.

causal_model_intervened ::
  (MonadDistribution m) =>
  Param m ->
  Smoke ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal (Stress, Smoke, Cancer))
causal_model_intervened params smoke_int {--} smoke_key =
  let model = causal_model params {--} smoke_key
   in do_ smoke_key (Value smoke_int) "smoke_int" model

main_III :: IO ()
main_III = do
  -- IO
  print "=== III- Causal Model intervened automatically with ChiRho do_ ==="
  smoke_key <- createKey
  let nSamples = 10000
  let model intv =
        let smoking_causal_model = causal_model_intervened default_params intv {--} smoke_key
         in let dist_model = getM smoking_causal_model empty
             in let transformed_model = do
                      -- m
                      entry <- dist_model
                      let (ns, table) = getMultiVal entry
                      let (_, _, cancer_c) = table (constWorld True ns)
                      return $ (if cancer_c == Cancer True then 1.0 else 0.0)
                 in transformed_model
  true_avg <- inferAvg nSamples (model (Smoke True))
  false_avg <- inferAvg nSamples (model (Smoke False))
  print "P(cancer | do(smoking)) [using `do_`]: "
  print true_avg
  print "P(cancer | do(not smoking)) [using `do_`]: "
  print false_avg
  print "This should match the results from section II."

{- Priors over parameters -}

parameter_prior ::
  (MonadDistribution m) =>
  m (Param m)
parameter_prior = do
  -- m
  pStress <- beta 1 1
  pSmokingGivenStress <- beta 1 1
  pSmokingGivenNoStress <- beta 1 1
  pCancerGivenStressAndSmoking <- beta 1 1
  pCancerGivenStressAndNoSmoking <- beta 1 1
  pCancerGivenNoStressAndSmoking <- beta 1 1
  pCancerGivenNoStressAndNoSmoking <- beta 1 1
  return
    ( Stress <$> bernoulli pStress,
      \(Stress stress) -> if stress then Smoke <$> bernoulli pSmokingGivenStress else Smoke <$> bernoulli pSmokingGivenNoStress,
      \(Stress stress) (Smoke smoking) -> case (stress, smoking) of
        (True, True) -> Cancer <$> bernoulli pCancerGivenStressAndSmoking
        (True, False) -> Cancer <$> bernoulli pCancerGivenStressAndNoSmoking
        (False, True) -> Cancer <$> bernoulli pCancerGivenNoStressAndSmoking
        (False, False) -> Cancer <$> bernoulli pCancerGivenNoStressAndNoSmoking
    )

-- Empirical observations

-- [Theo] where do these scores come from?

scores :: (Stress, Smoke, Cancer) -> Double
scores (Stress True, Smoke True, Cancer True) = 0.05
scores (Stress True, Smoke True, Cancer False) = 0.2
scores (Stress True, Smoke False, Cancer True) = 0.05
scores (Stress True, Smoke False, Cancer False) = 0.2
scores (Stress False, Smoke True, Cancer True) = 0.05
scores (Stress False, Smoke True, Cancer False) = 0.2
scores (Stress False, Smoke False, Cancer True) = 0.05
scores (Stress False, Smoke False, Cancer False) = 0.2

causal_population_model ::
  (MonadDistribution m) =>
  Param m ->
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
causal_population_model params n_individuals {--} smoke_key =
  let list = replicate n_individuals (causal_model params smoke_key)
   in sequenceA <$> (sequenceA list)

bayesian_population_model ::
  (MonadDistribution m) =>
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
bayesian_population_model n_individuals {--} smoke_key = do
  -- Caus
  params <- lift parameter_prior
  causal_population_model params n_individuals smoke_key

main_IV_A :: IO ()
main_IV_A = do
  -- IO
  print "=== IV-A: Observational Data with (Bayesian) Uncertain Parameters ==="
  smoke_key <- createKey
  let model = bayesian_population_model 100 smoke_key
  let dist_model = getM model empty
  -- Get estimate of P(cancer | smoking) and P(cancer | not smoking)
  let infer_model = do
        -- m
        individuals <- dist_model
        let (ns, table) = getMultiVal individuals
        let observational = table (constWorld False ns)
        let smokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- observational, smoke == True && cancer == True]
        let notSmokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- observational, smoke == False && cancer == True]
        let smokes = length [() | (_, Smoke smoke, _) <- observational, smoke == True]
        return
          ( fromIntegral smokingAndCancer / fromIntegral smokes,
            fromIntegral notSmokingAndCancer / fromIntegral (length observational - smokes)
          )
  let nExperiments = 5000
  results <- replicateM nExperiments (sampleIO infer_model)
  -- print (results)
  let (smoke_trues, smoke_falses) = unzip results
  let smoke_trues_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_trues
  let smoke_falses_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_falses
  let filename = "IV_A_smoking_effects_bayesian.png"
  print ("Plotting Smoking Effects under Bayesian Prior and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues_trunc smoke_falses_trunc ("Smoking Effects on Cancer Probability under Bayesian Prior" ++ " (n=" ++ show nExperiments ++ ")")
  pltResult <- file ("output/" ++ filename) $ plt
  print pltResult

intervened_causal_population_model ::
  (MonadDistribution m) =>
  Param m ->
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
intervened_causal_population_model params n_individuals {--} smoke_key =
  let list =
        replicate (n_individuals `div` 2) (causal_model_intervened params (Smoke True) smoke_key)
          ++ replicate (n_individuals `div` 2) (causal_model_intervened params (Smoke False) smoke_key)
   in sequenceA <$> sequenceA list


intervened_bayesian_population_model ::
  (MonadDistribution m) =>
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
intervened_bayesian_population_model n_individuals {--} smoke_key = do
  -- Caus
  params <- lift parameter_prior
  intervened_causal_population_model params n_individuals smoke_key

main_IV_B :: IO ()
main_IV_B = do
  -- IO
  print "=== IV-B: Intervened Data with (Bayesian) Uncertain Parameters ==="
  smoke_key <- createKey
  let nIndividuals = 100
  let model = intervened_bayesian_population_model nIndividuals smoke_key
  let dist_model = getM model empty
  -- Get estimate of P(cancer | smoking) and P(cancer | not smoking)
  let infer_model = do
        -- m
        individuals <- dist_model
        let (ns, table) = getMultiVal individuals
        let interventional = table (constWorld True ns)
        let smokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- interventional, smoke == True && cancer == True]
        let notSmokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- interventional, smoke == False && cancer == True]
        let smokes = length [() | (_, Smoke smoke, _) <- interventional, smoke == True]
        return
          ( fromIntegral smokingAndCancer / fromIntegral smokes,
            fromIntegral notSmokingAndCancer / fromIntegral (length interventional - smokes),
            ate interventional
          )
  let nExperiments = 5000
  results <- replicateM nExperiments (sampleIO infer_model)
  let resultsFiltered = filter (\(x, y, z) -> not (isNaN x) && not (isNaN y) && not (isNaN z)) results
  -- print (results)
  let (smoke_trues, smoke_falses, ate_estimates) = unzip3 resultsFiltered
  -- let smoke_trues_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_trues
  -- let smoke_falses_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_falses
  let filename = "IV_B_smoking_effects_bayesian_intervened.png"
  print ("Plotting Smoking Effects *after intervention* under Bayesian Prior and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues smoke_falses ("Smoking Intervention Effects on Cancer Probability under Bayesian Prior" ++ " (n=" ++ show nExperiments ++ ")")
  pltResult <- file ("output/" ++ filename) plt
  print pltResult
  -- Compute ATE
  let filename2 = "IV_B_ate_estimate.png"
  print ("Plotting ATE estimates and saving in " ++ filename2)
  let plt2 = visualise_ate 25 ate_estimates "Interventional Data - Uncertain Parameters"
  pltResult2 <- file ("output/" ++ filename2) plt2
  print pltResult2

type AltParam m = (m Stress, Stress -> m Cancer, Stress -> Cancer -> m Smoke)

-- Uncertainty over model structure.
alt_causal_model ::
  (MonadDistribution m) =>
  AltParam m ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal (Stress, Smoke, Cancer))
alt_causal_model (stress_prob, cancer_cond_prob, smokes_cond_prob) smoke_key = do
  -- Caus
  stress <- lift $ sample (pure stress_prob)
  cancer <- lift $ sample (cancer_cond_prob <$> stress)
  smoke <- lift $ sample (smokes_cond_prob <$> stress <*> cancer)
  smokeInt <- new_ smoke_key smoke
  return $ (,,) <$> stress <*> smokeInt <*> cancer -- [Theo] Not sure if broadcasting here breaks sth. At least make sure not to postcompose this with anything, because this WILL break.

alt_default_params :: (MonadDistribution m) => AltParam m
alt_default_params = (stress_prob, cancer_cond_prob, smokes_cond_prob)
  where
    stress_prob = Stress <$> bernoulli 0.5

    cancer_cond_prob (Stress True) = Cancer <$> bernoulli 0.8
    cancer_cond_prob (Stress False) = Cancer <$> bernoulli 0.2

    smokes_cond_prob (Stress True) (Cancer True) = Smoke <$> bernoulli 0.85
    smokes_cond_prob (Stress True) (Cancer False) = Smoke <$> bernoulli 0.8
    smokes_cond_prob (Stress False) (Cancer True) = Smoke <$> bernoulli 0.15
    smokes_cond_prob (Stress False) (Cancer False) = Smoke <$> bernoulli 0.1

alt_population_model ::
  (MonadDistribution m) =>
  AltParam m ->
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
alt_population_model params n_individuals {--} smoke_key =
  let list = replicate n_individuals (alt_causal_model params smoke_key)
   in sequenceA <$> (sequenceA list)

alt_parameter_prior ::
  (MonadDistribution m) =>
  m (AltParam m)
alt_parameter_prior = do
  -- m
  pStress <- beta 1 1
  pCancerGivenStress <- beta 1 1
  pCancerGivenNoStress <- beta 1 1
  pSmokingGivenStressAndCancer <- beta 1 1
  pSmokingGivenStressAndNoCancer <- beta 1 1
  pSmokingGivenNoStressAndCancer <- beta 1 1
  pSmokingGivenNoStressAndNoCancer <- beta 1 1
  return
    ( Stress <$> bernoulli pStress,
      \(Stress stress) -> if stress then Cancer <$> bernoulli pCancerGivenStress else Cancer <$> bernoulli pCancerGivenNoStress,
      \(Stress stress) (Cancer cancer) -> case (stress, cancer) of
        (True, True) -> Smoke <$> bernoulli pSmokingGivenStressAndCancer
        (True, False) -> Smoke <$> bernoulli pSmokingGivenStressAndNoCancer
        (False, True) -> Smoke <$> bernoulli pSmokingGivenNoStressAndCancer
        (False, False) -> Smoke <$> bernoulli pSmokingGivenNoStressAndNoCancer
    )

alt_bayesian_population_model ::
  (MonadDistribution m) =>
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
alt_bayesian_population_model n_individuals {--} smoke_key = do
  -- Caus
  params <- lift alt_parameter_prior
  alt_population_model params n_individuals smoke_key

main_V_A :: IO ()
main_V_A = do
  -- IO
  print "=== V-A: Alt Causal Structure: Observational Data with (Bayesian) Uncertain Parameters ==="
  smoke_key <- createKey
  let model = alt_bayesian_population_model 100 smoke_key
  let dist_model = getM model empty
  -- Get estimate of P(cancer | smoking) and P(cancer | not smoking)
  let infer_model = do
        -- m
        individuals <- dist_model
        let (ns, table) = getMultiVal individuals
        let observational = table (constWorld False ns)
        let smokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- observational, smoke == True && cancer == True]
        let notSmokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- observational, smoke == False && cancer == True]
        let smokes = length [() | (_, Smoke smoke, _) <- observational, smoke == True]
        return
          ( fromIntegral smokingAndCancer / fromIntegral smokes,
            fromIntegral notSmokingAndCancer / fromIntegral (length observational - smokes)
          )
  let nExperiments = 5000
  results <- replicateM nExperiments (sampleIO infer_model)
  -- print (results)
  let (smoke_trues, smoke_falses) = unzip results
  let smoke_trues_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_trues
  let smoke_falses_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_falses
  let filename = "V_A_alt_smoking_effects_bayesian.png"
  print ("Plotting Smoking Effects under Bayesian Prior and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues_trunc smoke_falses_trunc ("Alt Causal Structure: Smoking Effects on Cancer Probability under Bayesian Prior" ++ " (n=" ++ show nExperiments ++ ")")
  pltResult <- file ("output/" ++ filename) plt
  print pltResult

alt_intervened_causal_model ::
  (MonadDistribution m) =>
  AltParam m ->
  Smoke ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal (Stress, Smoke, Cancer))
alt_intervened_causal_model params smoke_int {--} smoke_key =
  let model = alt_causal_model params {--} smoke_key
   in do_ smoke_key (Value smoke_int) "smoke_int" model

alt_intervened_causal_population_model ::
  (MonadDistribution m) =>
  AltParam m ->
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
alt_intervened_causal_population_model params n_individuals {--} smoke_key =
  let list =
        replicate (n_individuals `div` 2) (alt_intervened_causal_model params (Smoke True) smoke_key)
          ++ replicate (n_individuals `div` 2) (alt_intervened_causal_model params (Smoke False) smoke_key)
   in sequenceA <$> sequenceA list

alt_intervened_bayesian_population_model ::
  (MonadDistribution m) =>
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
alt_intervened_bayesian_population_model n_individuals smoke_key = do
  -- Caus
  params <- lift alt_parameter_prior
  alt_intervened_causal_population_model params n_individuals smoke_key

  -- Testing alternative way of constructing the same model???? [WORKED]
--   let model = alt_causal_model params smoke_key
--   let replicated_model = sequenceA <$> replicateM (n_individuals `div` 2) model
--   let true_model = do_ smoke_key (Value (Smoke True)) "smoke_int" replicated_model
--   let false_model = do_ smoke_key (Value (Smoke False)) "smoke_int" replicated_model
--   let replicated_false_model = sequenceA <$> replicateM (n_individuals `div` 2) false_model

-- The following fails: if I put in the "alt_bayesian_population_model" and replicate it is break and gives the wrong answer.
-- Mistake here is that I effectively create a different Parametric Prior for each individual rather than sharing it across the population.
-- In ChiRho this sharing of parametric prior is possible because replicated models have the intervention points replicated too *instead of copied*,
-- so we can intervene on all of them at once while not duplicating the parametric priors.
--   let model = alt_bayesian_population_model (n_individuals `div` 2) smoke_key
--   let true_model = do_ smoke_key (Value (Smoke True)) "smoke_int" model
--   let false_model = do_ smoke_key (Value (Smoke False)) "smoke_int" model
--   Shared code.
--   true_list <- true_model
--   false_list <- false_model
--   let combined = (++) <$> true_list <*> false_list
--   return combined

main_V_B :: IO ()
main_V_B = do
  -- IO
  print "=== V-B: Intervened Data with (Bayesian) Uncertain Parameters ==="
  smoke_key <- createKey
  let nIndividuals = 1000
  let model = alt_intervened_bayesian_population_model nIndividuals smoke_key
  let dist_model = getM model empty
  -- Get estimate of P(cancer | smoking) and P(cancer | not smoking)
  let infer_model = do
        -- m
        individuals <- dist_model
        let (ns, table) = getMultiVal individuals
        let interventional = table (constWorld True ns)
        let smokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- interventional, smoke == True && cancer == True]
        let notSmokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- interventional, smoke == False && cancer == True]
        let smokes = length [() | (_, Smoke smoke, _) <- interventional, smoke == True]
        return
          ( fromIntegral smokingAndCancer / fromIntegral smokes,
            fromIntegral notSmokingAndCancer / fromIntegral (length interventional - smokes),
            ate interventional
          )
  let nExperiments = 5000
  results <- replicateM nExperiments (sampleIO infer_model)
  let resultsFiltered = filter (\(x, y, z) -> not (isNaN x) && not (isNaN y) && not (isNaN z)) results
  -- print (results)
  let (smoke_trues, smoke_falses, ate_estimates) = unzip3 resultsFiltered
  -- let smoke_trues_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_trues
  -- let smoke_falses_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_falses
  let filename = "V_B_alt_smoking_effects_bayesian_intervened.png"
  print ("Plotting Smoking Effects *after intervention* under Bayesian Prior and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues smoke_falses ("Alt Structure Smoking Intervention Effects on Cancer Probability under Bayesian Prior" ++ " (n=" ++ show nExperiments ++ ")")
  pltResult <- file ("output/" ++ filename) plt
  print pltResult
  -- Compute ATE
  let filename2 = "V_B_alt_ate_estimate.png"
  print ("Plotting ATE estimates and saving in " ++ filename2)
  let plt2 = visualise_ate 25 ate_estimates "Interventional Data - Uncertain Parameters - Alternative structure"
  pltResult2 <- file ("output/" ++ filename2) plt2
  print pltResult2

population_causal_model_uncertain_structure ::
  (MonadDistribution m) =>
  Param m ->
  AltParam m ->
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
population_causal_model_uncertain_structure params alt_params n_individuals {--} smoke_key = do
    -- Caus
  result <- causal_population_model params n_individuals smoke_key
  alt_result <- alt_population_model alt_params n_individuals smoke_key
  is_original_model <- sample (pure $ bernoulli 0.5)
  let branchWiseComputation isOriginalModel res altRes =
        if isOriginalModel then res else altRes
  let combinedResults = branchWiseComputation <$> is_original_model <*> result <*> alt_result
  return combinedResults


bayesian_population_causal_model_uncertain_structure ::
  (MonadDistribution m) =>
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
bayesian_population_causal_model_uncertain_structure n_individuals {--} smoke_key = do
  -- Caus
--   result <- bayesian_population_model n_individuals smoke_key
--   alt_result <- alt_bayesian_population_model n_individuals smoke_key
--   -- Here we could do model averaging based on model evidence
--   -- Not sure this is the right thing btw need to check.
-- --   is_original_model <- sample (pure $ replicateM n_individuals (bernoulli 0.5))
-- --   -- Combine them.
-- --   let branchWiseComputation isOriginalModel res altRes =
-- --         let zipped = zip3 isOriginalModel res altRes in
-- --         [ if isOrig then r else ar
-- --           | (isOrig, r, ar) <- zipped
-- --         ]
-- --   let combinedResults = branchWiseComputation <$> is_original_model <*> result <*> alt_result
-- --   let combinedResults = branchWiseComputation <$> is_original_model <*> result <*> alt_result
--   return alt_result
    params <- lift parameter_prior
    alt_params <- lift alt_parameter_prior
    population_causal_model_uncertain_structure params alt_params n_individuals smoke_key

main_VI_A :: IO ()
main_VI_A = do
  -- IO
  print "=== VI-A: Alt Causal Structure: Observational Data with (Bayesian) Uncertain Structure AND Parameters ==="
  smoke_key <- createKey
  let model = bayesian_population_causal_model_uncertain_structure 100 smoke_key
  let dist_model = getM model empty
  -- Get estimate of P(cancer | smoking) and P(cancer | not smoking)
  let infer_model = do
        -- m
        individuals <- dist_model
        let (ns, table) = getMultiVal individuals
        let observational = table (constWorld False ns)
        let smokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- observational, smoke == True && cancer == True]
        let notSmokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- observational, smoke == False && cancer == True]
        let smokes = length [() | (_, Smoke smoke, _) <- observational, smoke == True]
        return
          ( fromIntegral smokingAndCancer / fromIntegral smokes,
            fromIntegral notSmokingAndCancer / fromIntegral (length observational - smokes)
          )
  let nExperiments = 5000
  results <- replicateM nExperiments (sampleIO infer_model)
  -- print (results)
  let (smoke_trues, smoke_falses) = unzip results
  let smoke_trues_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_trues
  let smoke_falses_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_falses
  let filename = "VI_A_smoking_effects_observational_uncertain_structure_and_params.png"
  print ("Plotting Smoking Effects under Bayesian Params and Uncertain Structure and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues_trunc smoke_falses_trunc ("Smoking Effects on Cancer Probability under Bayesian Prior + Uncertain Structure" ++ " (n=" ++ show nExperiments ++ ")")
  pltResult <- file ("output/" ++ filename) plt
  print pltResult

bayesian_intervened_causal_model_uncertain_structure ::
  (MonadDistribution m) =>
  Int ->
  InterventionPointKey m Smoke ->
  Caus m (MultiVal [(Stress, Smoke, Cancer)])
bayesian_intervened_causal_model_uncertain_structure n_individuals smoke_key =     do -- Caus
    -- Here the ChiRho tutorial does this intervention with a tensor of assignments; I don't see an 
    -- obvious way to do so in this implementation so I use an alternative way.
    prior <- lift parameter_prior
    alt_prior <- lift alt_parameter_prior
    let model = population_causal_model_uncertain_structure prior alt_prior (n_individuals `div` 2) smoke_key
    let trueModel = do_ smoke_key (Value (Smoke True)) "smoking" model
    let falseModel = do_ smoke_key (Value (Smoke False)) "smoking" model
    -- Combined model
    let combinedModel = do -- Caus
            trueBranch <- trueModel
            falseBranch <- falseModel
            let combinedList = (++) <$> trueBranch <*> falseBranch
            return combinedList
    combinedModel

main_VI_B :: IO ()
main_VI_B = do -- IO
  print "=== VI-B: Intervened Data with (Bayesian) Uncertain Parameters and Uncertain Structure ==="
  smoke_key <- createKey
  let nIndividuals = 1000
  let model = bayesian_intervened_causal_model_uncertain_structure nIndividuals smoke_key
  let dist_model = getM model empty
  -- Get estimate of P(cancer | smoking) and P(cancer | not smoking)
  let infer_model = do
        -- m
        individuals <- dist_model
        let (ns, table) = getMultiVal individuals
        -- Get the intervened data
        let interventional = table (constWorld True ns)
        let smokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- interventional, smoke == True && cancer == True]
        let notSmokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- interventional, smoke == False && cancer == True]
        let smokes = length [() | (_, Smoke smoke, _) <- interventional, smoke == True]
        return
          ( fromIntegral smokingAndCancer / fromIntegral smokes,
            fromIntegral notSmokingAndCancer / fromIntegral (length interventional - smokes),
            ate interventional
          )
  let nExperiments = 5000
  results <- replicateM nExperiments (sampleIO infer_model)
  let resultsFiltered = filter (\(x, y, z) -> not (isNaN x) && not (isNaN y) && not (isNaN z)) results
  print (length resultsFiltered)
  let (smoke_trues, smoke_falses, ate_estimates) = unzip3 resultsFiltered
  -- let smoke_trues_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_trues
  -- let smoke_falses_trunc = map (\x -> if x < 1e-4 || isNaN x then 0 else x) smoke_falses
  let filename = "VI_B_smoking_effects_uncertain_structure_and_params_intervened.png"
  print ("Plotting Smoking Effects *after intervention* under Bayesian Prior and Uncertain Structure and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues smoke_falses ("Uncertain Structure and Parameters: Smoking Intervention Effects on Cancer Probability" ++ " (n=" ++ show nExperiments ++ ")")
  pltResult <- file ("output/" ++ filename) plt
  print pltResult
  -- Compute ATE
  let filename2 = "VI_B_ate_estimate_uncertain_structure.png"
  print ("Plotting ATE estimates and saving in " ++ filename2)
  let plt2 = visualise_ate 500 ate_estimates "Interventional Data - Uncertain Parameters - Uncertain structure"
  pltResult2 <- file ("output/" ++ filename2) plt2
  print pltResult2

-- Final part: "Causal Inference is Bayesian Inference"
-- main_VII


scoreOf :: (Stress, Smoke, Cancer) -> Log Double
scoreOf (Stress True, Smoke True, Cancer True) = (0.5 * 0.6 * 0.9)
scoreOf (Stress True, Smoke True, Cancer False) = (0.5 * 0.6 * 0.1)
scoreOf (Stress True, Smoke False, Cancer True) =  (0.5 * 0.4 * 0.1)
scoreOf (Stress True, Smoke False, Cancer False) =  (0.5 * 0.4 * 0.9)
scoreOf (Stress False, Smoke True, Cancer True) =  (0.5 * 0.3 * 0.3)
scoreOf (Stress False, Smoke True, Cancer False) =  (0.5 * 0.3 * 0.7)
scoreOf (Stress False, Smoke False, Cancer True) =  (0.5 * 0.7 * 0.1)
scoreOf (Stress False, Smoke False, Cancer False) = (0.5 * 0.7 * 0.9)


main_VII = do
    print "=== VII: Causal Inference is Bayesian Inference ==="
    smoke_key1 <- createKey
    smoke_key2 <- createKey
    smoke_key3 <- createKey
    -- Results
    -- print "Sampling Prior Distribution"
    -- let nExperiments1 = 5000
    -- results1 <- replicateM nExperiments1 (sampleIO (priorATE smoke_key1))
    -- let prior_ATE_estimates = filter (not . isNaN) results1
    -- print "Sampling True Distribution"
    -- let nExperiments3 = 5000
    -- results3 <- replicateM nExperiments3 (sampleIO (trueATE smoke_key3))
    -- let true_ATE_estimates = filter (not . isNaN) results3
    print "Estimating Posterior Distribution"
    -- let nExperiments2 = 1
    -- samples <- mh 0.4 $ posteriorATE smoke_key2
    -- samples <- replicateM nExperiments2 (inferAvg 10000000 (posteriorATE smoke_key2))
    samples <- sampler $ mcmc MCMCConfig
                    {numMCMCSteps = 10000, 
                    proposal = SingleSiteMH, 
                    numBurnIn = 9000} $ posteriorATE smoke_key2
    -- samples <- sampler $ weighted $ posteriorATE smoke_key2
    -- samples <-  sampler $ mh 0.4 (posteriorATE smoke_key2)
    -- let samplesMean = sum samples / fromIntegral (length samples)
    -- return samplesMean
    let (smokeTrues, smokeFalses, ateEstimates) = unzip3 samples
    -- let ateEstimates2 = (-) <$> smokeTrues <*> smokeFalses
    -- samples <- replicateM 100 comp
    -- let estimates = take nExperiments2 $ map (snd . fst) samples
    -- Plotting
    let filename = "VII_ate_estimates_causal_inference_is_bayesian.png"
    print ("Plotting ATE estimates and saving in " ++ filename)
    -- let nrBins :: Int = 10
    let plt = visualise_smoking_effects 10 smokeTrues smokeFalses "Causal Inference is Bayesian Inference"
    pltResult <- file ("output/" ++ filename) plt
    print pltResult
    let plt2 = visualise_ate 10 ateEstimates "Causal Inference is Bayesian Inference"
    let filename2 = "VII_ate_estimates_causal_inference_is_bayesian_ate.png"
    print ("Plotting ATE estimates and saving in " ++ filename2)
    pltResult2 <- file ("output/" ++ filename2) plt2
    print pltResult2
    -- let plt3 = visualise_ate nrBins ateEstimates2 "Causal Inference is Bayesian Inference"
    -- let filename3 = "VII_ate_estimates_causal_inference_is_bayesian_ate_2.png"
    -- print ("Plotting ATE estimates and saving in " ++ filename3)
    -- pltResult3 <- file ("output/" ++ filename3) plt3
    -- print pltResult3
    -- let plt = mp
    --         -- % histogram prior_ATE_estimates nrBins @@ [o2 "density" True, o2 "histtype" "stepfilled", o2 "alpha" (0.8 :: Double), o2 "label" "Prior ATE"]
    --         % histogram samples nrBins @@ [o2 "density" True, o2 "histtype" "stepfilled", o2 "alpha" (0.8 :: Double), o2 "label" "Posterior ATE"]
    --         -- % histogram true_ATE_estimates nrBins @@ [o2 "density" True, o2 "histtype" "stepfilled", o2 "alpha" (0.3 :: Double), o2 "label" "True ATE"]
    --         % xlabel "Average Treatment Effect (ATE)"
    --         % ylabel "Density"
    --         % legend
    --         % title "ATE: prior, posterior and true distributions"
    -- pltResult <- file ("output/" ++ filename) plt
    -- print pltResult
    where
        priorATE :: (MonadDistribution m) => InterventionPointKey m Smoke -> m Double
        priorATE smoke_key = do
            results <- getM (intervened_bayesian_population_model 1000 smoke_key) empty
            let (ns, table) = getMultiVal results
            let interventional = table (constWorld True ns)
            return $ ate interventional
        trueATE :: (MonadDistribution m) => InterventionPointKey m Smoke -> m Double
        trueATE smoke_key = do
            individuals <- getM (intervened_causal_population_model data_params 1000 smoke_key) empty
            let (ns, table) = getMultiVal individuals
            let interventional = table (constWorld True ns)
            return $ ate interventional
        posteriorPredDistribution :: (MonadMeasure m) => InterventionPointKey m Smoke -> m (MultiVal [(Stress, Smoke, Cancer)])
        posteriorPredDistribution smoke_key = do
            -- Generate "data"
            let n_invididuals = 500
            -- let model = causal_population_model data_params n_invididuals smoke_key
            -- let dist_model = getM model empty
            -- individuals <- dist_model
            -- let (ns, table) = getMultiVal individuals
            -- let observational = table (constWorld False ns)
            -- let dist = toEmpiricalWeighted observational
            -- prior model
            param <- parameter_prior
            let bayesian_population_counterfactual_model = intervened_causal_population_model param n_invididuals smoke_key
            samples <- getM bayesian_population_counterfactual_model empty
            -- conditionFactual (== observational) samples
            -- scoreFactual (foldl (\acc x -> acc * scoreOf x) 1) samples
            let (ns, table) = getMultiVal samples
            let factual = table (constWorld False ns)
            forM_ factual $ \feature ->
                score (scoreOf feature)
            -- @Dario: WHICH ONE TO DO between the following 2:
            return samples
            -- getM bayesian_population_counterfactual_model empty
        posteriorATE :: (MonadMeasure m) => InterventionPointKey m Smoke -> m (Double, Double, Double)
        posteriorATE smoke_key = do
            individuals <- posteriorPredDistribution smoke_key
            let (ns, table) = getMultiVal individuals
            let interventional = table (constWorld True ns)
            let smokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- interventional, smoke == True && cancer == True]
            let notSmokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- interventional, smoke == False && cancer == True]
            let smokes = length [() | (_, Smoke smoke, _) <- interventional, smoke == True]
            return
                ( fromIntegral smokingAndCancer / fromIntegral smokes,
                    fromIntegral notSmokingAndCancer / fromIntegral (length interventional - smokes),
                    ate interventional)

-- main_VII_B :: IO ()
-- main_VII_B = do
--   -- IO
--   smoke_key <- createKey
--   -- parameterised model
--   let model = do
--         param <- lift parameter_prior
--         origModel <- causal_model_intervened param (Smoke True) smoke_key
--         return _
--   return _

main_IV :: IO ()
main_IV = do
  -- IO
  smoke_key <- createKey
  let model = do_ smoke_key (Value (Smoke True)) "smoking" (causal_population_model default_params 100 smoke_key)
  let dist_model = getM model empty
  let infer_model branch = do
        -- m
        individuals <- dist_model
        let (ns, table) = getMultiVal individuals
        let features = table (constWorld branch ns)
        return (ate features)
  avg_ate_real <- inferAvg 10000 (infer_model False)
  print "ATE (observed): "
  print avg_ate_real
  avg_ate_cf <- inferAvg 10000 (infer_model True)
  print "ATE (counterfactual): "
  print avg_ate_cf

main_V :: IO ()
main_V = do
  -- IO
  smoke_key <- createKey
--   let model = do_ smoke_key (Value (Smoke True)) "smoking" (bayesian_population_model 100 smoke_key)
  let model = intervened_bayesian_population_model 1000 smoke_key
  let dist_model = getM model empty
  let infer_model = do
        -- m
        individuals <- dist_model
        let (ns, table) = getMultiVal individuals
        let real_features = table (constWorld False ns)
        let cf_features = table (constWorld True ns)
        forM_ real_features $ \feature ->
        --   score (Exp (log (scores feature)))
            score (scoreOf feature)
        individuals2 <- dist_model
        let (ns2, table2) = getMultiVal individuals2
        return (ate $ table2 (constWorld True ns2))
  avg_ate_bayesian <- inferAvg 10000 infer_model
  print "ATE (counterfactual, bayesian):"
  print avg_ate_bayesian

{-
High level:

Given:
empirical distribution OBS :: m (Stress, Smoking, Cancer)
(generate this from synthetic data)

-------
parameters <- parameter_prior

[x1, ..., x100] <- 100 samples from model(parameters)
for i = 1,..,100 do
    score(OBS(x_i))

--- is actually the same as ---

for i = 1,...,100 do
    x <- model(parameters)
    score(OBS(x))

return parameters
-----

forall p : X -> [0,1] discrete distribution

(score p(x)) // soft condition
==
(y <- p; condition(y == x)) // hard condition

1. generative model that generates random variables (Z,X)
2. condition on data `x`, i.e. compute P(Z|X = x), i.e. condition(X == x)
3. for effiency, try to rewrite this using soft conditions (if somewhere X <- p; ...; condition(X = x)) ~> (score p(x))
4. profit

-- conditioning on multiple variables

(X1,...,Xn) <- 100 samples from model
condition (forall i, Xi = xi)
-}

main :: IO ()
main = do
  print "[TODO] This example needs cleaning up; please refer to the individual main_X functions to run them."
  --   print "--------------- Smoking Model: \"Observation 1\" ---------------"
  --   main_I -- OK
  -- main_II -- OK
  --   main_III -- OK
  -- print "--------------- Smoking Model: \"Observation 2\" ---------------"
  --   main_IV_A -- OK
  --   main_IV_B -- OK
  -- main_V_A -- OK
--   main_V_B -- OK
--   main_VI_A -- OK
--   main_VI_B -- OK
  -- print "--------------- Smoking Model: \"Observation 3\" ---------------"
  -- main_VII -- Everything ok apart from "main task" of inference.
--   let i :: Log Double = 0.1
--   let j :: Log Double = 0.1
--   let k = Exp (log (i * j))
--   print $ i * j
--   print k
--   print $ exp $ ln (i * j)
  -- main_IV -- not sure but looks ok
  -- main_V --  weird unless it's expected.
