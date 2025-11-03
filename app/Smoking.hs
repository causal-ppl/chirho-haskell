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
import Control.Monad.Bayes.Enumerator (toEmpiricalWeighted, toEmpirical, enumerateToDistribution, Enumerator, mass)
import Control.Monad.Bayes.Inference.MCMC
import System.CPUTime
import Text.Printf


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
-}

{- Observation 1: causal models are probabilistic programs -}

newtype Stress = Stress Bool deriving (Eq, Show, Ord)

newtype Smoke = Smoke Bool deriving (Eq, Show, Ord)

newtype Cancer = Cancer Bool deriving (Eq, Show, Ord)

-- empirical average treatment effect
ate :: [(Stress, Smoke, Cancer)] -> Double
ate sample_ = (fromIntegral cancer_and_smokes) / (fromIntegral smokes) - (fromIntegral cancer_and_not_smokes) / (fromIntegral not_smokes)
  where
    cancer_and_smokes = 1 + length [() | (Stress stress, Smoke smoke, Cancer cancer) <- sample_, smoke == True && cancer == True]
    smokes = 1 + length [() | (Stress stress, Smoke smoke, Cancer cancer) <- sample_, smoke == True]
    cancer_and_not_smokes = 1 + length [() | (Stress stress, Smoke smoke, Cancer cancer) <- sample_, smoke == False && cancer == True]
    not_smokes = 1 + length [() | (Stress stress, Smoke smoke, Cancer cancer) <- sample_, smoke == False]

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

i_observational_data_fixed_parameters :: IO ()
i_observational_data_fixed_parameters = do
  -- To be more pedantic this is not quite what ChiRho does.
  -- we have approx of distr, they have distr of approx's
  print "--- I- Simulating Observational Data with Fixed Parameters ---"
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
  let filename = "I_observational_data_fixed_parameters.png"
  print ("Plotting Smoking Effects and saving in " ++ filename)
  let nExperiments = 500
  results <- replicateM nExperiments $ do
    -- print "Sampling for plot..."
    smoke_true <- inferAvg nSamples (model $ Smoke True)
    smoke_false <- inferAvg nSamples (model $ Smoke False)
    return (smoke_true, smoke_false)
  let (smoke_trues, smoke_falses) = unzip results
  -- Matplotlib make plot
  result <- file ("output/" ++ filename) $ visualise_smoking_effects 10 smoke_trues smoke_falses ("Observational Data - Fixed Parameters" ++ " (n=" ++ show nExperiments ++ ")")
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

ii_applying_an_intervention_manually :: IO ()
ii_applying_an_intervention_manually = do
  print "--- II- Applying an intervention (manually)  ---"
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
  pltResult1 <- file ("output/" ++ filename) $ visualise_smoking_effects 10 smoke_trues smoke_falses ("Intervened Data - Fixed Parameters" ++ " (n=" ++ show nExperiments ++ ")")
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

iii_transforming_causal_models_using_chirho :: IO ()
iii_transforming_causal_models_using_chirho = do
  -- IO
  print "--- III- Transforming Causal Models using ChiRho do_ ---"
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

{- Observation 2: causal uncertainty is probabilistic uncertainty -}

{- Priors over parameters -}
parameter_prior_enumerator ::
  (MonadDistribution m) =>
  m (Param (Enumerator))
parameter_prior_enumerator = do
  -- m
  pStress <- beta 1 1
  pSmokingGivenStress <- beta 1 1
  pSmokingGivenNoStress <- beta 1 1
  pCancerGivenStressAndSmoking <- beta 1 1
  pCancerGivenStressAndNoSmoking <- beta 1 1
  pCancerGivenNoStressAndSmoking <- beta 1 1
  pCancerGivenNoStressAndNoSmoking <- beta 1 1
  return
    (  Stress <$> bernoulli pStress,
      \(Stress stress) -> if stress then Smoke <$> bernoulli pSmokingGivenStress else Smoke <$> bernoulli pSmokingGivenNoStress,
      \(Stress stress) (Smoke smoking) -> case (stress, smoking) of
        (True, True) -> Cancer <$> bernoulli pCancerGivenStressAndSmoking
        (True, False) -> Cancer <$> bernoulli pCancerGivenStressAndNoSmoking
        (False, True) -> Cancer <$> bernoulli pCancerGivenNoStressAndSmoking
        (False, False) -> Cancer <$> bernoulli pCancerGivenNoStressAndNoSmoking
    )

param_enum_to_m :: (MonadDistribution m) => Param (Enumerator) -> Param m
param_enum_to_m (sProb, smokesProb, cancerProb) =
  (enumerateToDistribution sProb, enumerateToDistribution . smokesProb, \ stress -> enumerateToDistribution . cancerProb stress)

parameter_prior ::
  (MonadDistribution m) =>
  m (Param m)
parameter_prior = fmap param_enum_to_m parameter_prior_enumerator

{- Simulating Observational Data with Bayesian Uncertain Parameters -}

{- Causal Model over Population -}

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

iv_a_adding_uncertainty_over_model_parameters :: IO ()
iv_a_adding_uncertainty_over_model_parameters = do
  -- IO
  print "--- IV-A: Adding Uncertainty over Model Parameters ---"
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
  let filename = "IV_A_observational_data_uncertain_parameters.png"
  print ("Plotting Smoking Effects under Bayesian Prior and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues_trunc smoke_falses_trunc ("Observational Data - Uncertain Parameters" ++ " (n=" ++ show nExperiments ++ ")")
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

iv_b_simulating_interventional_data_with_uncertain_parameters :: IO ()
iv_b_simulating_interventional_data_with_uncertain_parameters = do
  -- IO
  print "--- IV-B: Simulating Interventional Data with Uncertain Parameters ---"
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
  let filename = "IV_B_interventional_data_uncertain_parameters.png"
  print ("Plotting Smoking Effects *after intervention* under Bayesian Prior and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues smoke_falses ("Interventional Data - Uncertain Parameters" ++ " (n=" ++ show nExperiments ++ ")")
  pltResult <- file ("output/" ++ filename) plt
  print pltResult
  -- Compute ATE
  let filename2 = "IV_B_ate_interventional_data_uncertain_parameters.png"
  print ("Plotting ATE estimates and saving in " ++ filename2)
  let plt2 = visualise_ate 25 ate_estimates "ATE: Interventional Data - Uncertain Parameters"
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

v_a_adding_uncertainty_over_model_structure :: IO ()
v_a_adding_uncertainty_over_model_structure = do
  -- IO
  print "--- V-A: Adding Uncertainty over Model Structure ---"
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
  let filename = "V_A_alt_observational_data_uncertain_parameters_alternative_structure.png"
  print ("Plotting Smoking Effects under Bayesian Prior and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues_trunc smoke_falses_trunc ("Observational Data - Uncertain Parameters - Alternative Structure" ++ " (n=" ++ show nExperiments ++ ")")
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


v_b_intervened_data_alternative_structure :: IO ()
v_b_intervened_data_alternative_structure = do
  -- IO
  print "--- V-B: Intervened Data with (Bayesian) Uncertain Parameters ---"
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
  let plt = visualise_smoking_effects 25 smoke_trues smoke_falses ("Interventional Data - Uncertain Parameters - Alternative Structure" ++ " (n=" ++ show nExperiments ++ ")")
  pltResult <- file ("output/" ++ filename) plt
  print pltResult
  -- Compute ATE
  let filename2 = "V_B_alt_ate_estimate.png"
  print ("Plotting ATE estimates and saving in " ++ filename2)
  let plt2 = visualise_ate 25 ate_estimates "ATE: Interventional Data - Uncertain Parameters - Alternative Structure"
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
    params <- lift parameter_prior
    alt_params <- lift alt_parameter_prior
    population_causal_model_uncertain_structure params alt_params n_individuals smoke_key

vi_a_simulating_observational_data_with_uncertain_structure_and_parameters :: IO ()
vi_a_simulating_observational_data_with_uncertain_structure_and_parameters = do
  -- IO
  print "--- VI-A: Simulating Observational Data with Uncertain Structure *and* Parameters ---"
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
  let filename = "VI_A_observational_data_uncertain_structure_and_parameters.png"
  print ("Plotting Smoking Effects under Bayesian Params and Uncertain Structure and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues_trunc smoke_falses_trunc ("Observational Data - Uncertain Parameters and Structure" ++ " (n=" ++ show nExperiments ++ ")")
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

vi_b_simulating_interventional_data_with_uncertain_structure_and_parameters :: IO ()
vi_b_simulating_interventional_data_with_uncertain_structure_and_parameters = do -- IO
  print "--- VI-B: Simulating Interventional Data with Uncertain Structure and Parameters ---"
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
  let filename = "VI_B_interventional_data_uncertain_parameters_and_structure.png"
  print ("Plotting Smoking Effects *after intervention* under Bayesian Prior and Uncertain Structure and saving in " ++ filename)
  -- Matplotlib make plot
  let plt = visualise_smoking_effects 25 smoke_trues smoke_falses ("Interventional Data - Uncertain Parameters and Structure" ++ " (n=" ++ show nExperiments ++ ")")
  pltResult <- file ("output/" ++ filename) plt
  print pltResult
  -- Compute ATE
  let filename2 = "VI_B_ate_estimate_uncertain_structure.png"
  print ("Plotting ATE estimates and saving in " ++ filename2)
  let plt2 = visualise_ate 500 ate_estimates "ATE: Interventional Data - Uncertain Parameters - Uncertain structure"
  pltResult2 <- file ("output/" ++ filename2) plt2
  print pltResult2


{- Observation 3: Causal Inference is Bayesian Inference -}

likelihood_ :: Param Enumerator -> (Stress, Smoke, Cancer) -> Log Double
likelihood_ params individual =
      let l = mass (stat_model params) individual in
        -- convert l to logdouble
         Exp (log l)


parameter_posterior_enumerator ::
  (MonadMeasure m) =>
  [(Stress, Smoke, Cancer)] ->
  m (Param Enumerator)
parameter_posterior_enumerator data_obs = do
  params <- parameter_prior_enumerator
  forM_ data_obs $ \individual -> do
    score $ likelihood_ params individual
  return params


vii_a_prior_distribution_and_true_ate :: IO ([Double], [Double])
vii_a_prior_distribution_and_true_ate = do
  print "VII-A: Prior Distribution and True ATE"
  smoke_key_prior <- createKey
  smoke_key_true <- createKey
  -- Results
  print "Sampling Prior Distribution"
  let n_experiments_prior = 5000
  results_prior <- replicateM n_experiments_prior (sampleIO (priorATE smoke_key_prior))
  let prior_ATE_estimates = filter (not . isNaN) results_prior
  print "Sampling True Distribution"
  let n_experiments_true = 5000
  results_true <- replicateM n_experiments_true (sampleIO (trueATE smoke_key_true))
  let true_ATE_estimates = filter (not . isNaN) results_true
  return (prior_ATE_estimates, true_ATE_estimates)
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


vii_b_infer_ate_est_and_plot :: IO [Double]
vii_b_infer_ate_est_and_plot = do
  print "--- VII-B: Infer ATE Estimate and Plot Posterior Distribution ---"
  let n_individuals = 100
  -- Generate "data"
  smoke_key <- createKey
  let model = causal_population_model data_params n_individuals smoke_key
  let dist_model = getM model empty
  individuals <- sampleIO dist_model
  let (ns, table) = getMultiVal individuals
  let observational = table (constWorld False ns)
  -- Estimate posterior
  print "Estimating Posterior Distribution"
  smoke_key2 <- createKey
  samples_ <- sampler $ mcmc MCMCConfig
                {numMCMCSteps = 50000,
                proposal = SingleSiteMH,
                numBurnIn = 2000} $ posteriorATE n_individuals observational smoke_key2
  let (smokeTrues, smokeFalses, ateEstimates) = unzip3 samples_
  let filename = "VII_interventional_data_posterior.png"
  print ("Plotting ATE estimates and saving in " ++ filename)
  let plt = visualise_smoking_effects 15 smokeTrues smokeFalses "Interventional Data - Posterior Estimates"
  pltResult <- file ("output/" ++ filename) plt
  print pltResult
  let plt2 = visualise_ate 15 ateEstimates "Interventional Data - Posterior ATE Estimates"
  let filename2 = "VII_ate_interventional_data_posterior.png"
  print ("Plotting ATE estimates and saving in " ++ filename2)
  pltResult2 <- file ("output/" ++ filename2) plt2
  print pltResult2
  return ateEstimates
  where
        posteriorPredDistributionReference :: (MonadMeasure m) => Int -> [(Stress, Smoke, Cancer)] -> InterventionPointKey m Smoke -> m (MultiVal [(Stress, Smoke, Cancer)])
        posteriorPredDistributionReference n_individuals data_ smoke_key = do
            param <- parameter_prior_enumerator
            let param_m = param_enum_to_m param
            let model = intervened_causal_population_model param_m n_individuals smoke_key
            val <- run model
            let (ns, table) = getMultiVal val
            let observational = table (constWorld False ns)
            condition (observational == data_)
            run model
        posteriorPredDistribution :: (MonadMeasure m) => Int -> [(Stress, Smoke, Cancer)] -> InterventionPointKey m Smoke -> m (MultiVal [(Stress, Smoke, Cancer)])
        posteriorPredDistribution n_individuals data_ smoke_key = do
            param <- parameter_posterior_enumerator data_
            let param_m = param_enum_to_m param
            let model = intervened_causal_population_model param_m n_individuals smoke_key
            run model
        posteriorATE :: (MonadMeasure m) =>  Int -> [(Stress, Smoke, Cancer)] -> InterventionPointKey m Smoke -> m (Double, Double, Double)
        posteriorATE n_individuals data_ smoke_key = do
            individuals <- posteriorPredDistribution n_individuals data_ smoke_key
            let (ns, table) = getMultiVal individuals
            let interventional = table (constWorld True ns)
            let smokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- interventional, smoke == True && cancer == True]
            let notSmokingAndCancer = length [() | (_, Smoke smoke, Cancer cancer) <- interventional, smoke == False && cancer == True]
            let smokes = length [() | (_, Smoke smoke, _) <- interventional, smoke == True]
            return
                ( fromIntegral smokingAndCancer / fromIntegral smokes,
                    fromIntegral notSmokingAndCancer / fromIntegral (length interventional - smokes),
                    ate interventional)


vii_c_plot_all :: [Double] -> [Double] -> [Double] -> IO ()
vii_c_plot_all prior_ate_estimates true_ate_estimates posterior_ate_estimates = do
  print "--- VII-C: Plot All ATE Distributions: Prior, Posterior and True ---"
  let nrBins :: Int = 10
  let filename = "VII_ate_prior_true_posterior.png"
  let plt = mp
          % histogram prior_ate_estimates nrBins @@ [o2 "density" True, o2 "histtype" "stepfilled", o2 "alpha" (0.8 :: Double), o2 "label" "Prior ATE"]
          % histogram posterior_ate_estimates nrBins @@ [o2 "density" True, o2 "histtype" "stepfilled", o2 "alpha" (0.8 :: Double), o2 "label" "Posterior ATE"]
          % histogram true_ate_estimates nrBins @@ [o2 "density" True, o2 "histtype" "stepfilled", o2 "alpha" (0.3 :: Double), o2 "label" "True ATE"]
          % xlabel "Average Treatment Effect (ATE)"
          % ylabel "Density"
          % legend
          % title "ATE: prior, posterior and true distributions"
  pltResult <- file ("output/" ++ filename) plt
  print pltResult


main :: IO ()
main = do
  print "=== Observation 1: causal models are probabilistic programs ==="
  i_observational_data_fixed_parameters
  ii_applying_an_intervention_manually
  iii_transforming_causal_models_using_chirho
  print "=== End of Observation 1 ==="
  print "=== Observation 2: causal uncertainty is probabilistic uncertainty ==="
  iv_a_adding_uncertainty_over_model_parameters
  iv_b_simulating_interventional_data_with_uncertain_parameters
  v_a_adding_uncertainty_over_model_structure
  v_b_intervened_data_alternative_structure
  vi_a_simulating_observational_data_with_uncertain_structure_and_parameters
  vi_b_simulating_interventional_data_with_uncertain_structure_and_parameters
  print "=== End of Observation 2 ==="
  print "=== Observation 3: causal inference is Bayesian inference ==="
  (prior_ate_estimates, true_ate_estimates) <- vii_a_prior_distribution_and_true_ate
  start <- getCPUTime
  posterior_ate_estimates <- vii_b_infer_ate_est_and_plot
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  vii_c_plot_all prior_ate_estimates true_ate_estimates posterior_ate_estimates
  print "=== End of Observation 3 ==="
