# Causal Probabilistic Programming Without Tears


We have the following types of interest

* `Params`: parameters for the model: This is a triple of conditional distributions specifying stress, smoking|stress and cancer|(stress,smoking)
* `Individual`: This is a triple `(stress, smoking, cancer)`


# Individual Models

An individual model is a straightforward sampling morphism `Params -> P Individual`.

# Population Model

We fix a population size `n_individuals=10000`. A population model is a morphism 

```haskell
population_model : Param -> P [Individual]
```

which returns a list of size `n_individuals`. These individuals are independently sampled from the individual model;

```haskell
population_model params = replicateM n_individuals (individual_model params)
```

# Hierarchical Bayesian Population Model

We can create a hierarchical Bayesian population model by putting a prior on the parameters

```haskell
bayesian_population_model : P [Individual]
bayesian_population_model = do
    params <- prior
    population_model params
```

Two observations
* if we returned `params` as an output, then the inidividuals would be conditionally independent given `params`. But we don't return `params`, so there is a latent conditional independence
* we could define `bayesian_individual_model : P Individual` in the same way, but this is not very useful. In particular, `bayesian_population_model` can **not** be defined in terms of `bayesian_individual_model` because we have no way of sharing the parameter `params`.

# Inference

We now want to condition the model on synthetic data. We fix some true parameters `true_params : Params` and want to "learn" the latent variable `params` in a way that hopefully brings it close to `true_params`. We can do this in two ways.

## Fixed synthetic data

> This is what is applicable in real-world statistics. It is also what the ChiRho tutorial does.

We have a fixed observed dataset `data* : [Individual]` of size `n_individuals`.
* In the real world, this would be the result of a survey or whatever.
* For synthetic data, sample `data* <- population_model true_params` once! Might as well take the resulting list, and hard code in our program.

We then condition our model output on that data, which will update the latent params. I.e.

```haskell
params <- prior
sample <- population_model params
condition(sample == data*)
return params
```

is the same as

```haskell
params <- posterior
return params
```

Conditioning on an equality of lists can be turned into a conjunction (loop) of conditions

```haskell
params <- prior
forM_ data* $ \obs -> do
  sample <- individual_model params
  condition(sample == obs)
```

This can be rewritten to a scoring program 

```haskell
params <- prior
forM_ data* $ \obs -> do
  score(likelihood params obs)
```

where `likelihood params obs` equals the pmf of `(individual_model params)` evaluated at `obs`.

The outcome of this analysis is a single posterior distribution `posterior* : P Params`.

## Expected synthetic data

This is the following program

```haskell
data <- population_model true_params
params <- prior
sample <- population_model params
condition(sample == data)
return params
```

Unlike in **fixed synthetic data**, here `data` is a random variable! The outcome of this analysis corresponds to computing the posteriors under all possible synthetic datasets, and averaging the results out to obtain `posterior : P Params`.

If the analysis performs well under all likely synthetic datasets, we should expect `posterior` to be similar to `posterior*`. Otherwise, it should have more variance (owing to the variance of the synthetic dataset).

Anyways, we again derive a scoring program, where first we rewrite

```haskell
params <- prior
replicateM n_individuals $ do
  obs <- individual_model params
  sample <- individual_model params
  condition(data == sample)
```

which is equal to

```haskell
params <- prior
replicateM n_individuals $ do
  obs <- individual_model params
  score(likelihood params obs)
```

which can be reduced to a single score under the expected likelihood. Not sure if this is what we want, but it should run just fine.

# Counterfactuals

To do a counterfactual analysis, we compute an enhanced version of our models that gives two outputs: a factual and counterfactual one. We can either does this by hand or via ChiRho automation.

For simplicity, in the individual_model, we take the desired intervention as an argument

```haskell
individual_model' : Params -> Smoking -> P (Individual, Individual)
individual_model' p smoking_int = do
  stress <- p.stress
  smoking <- p.smoking_given_stress stress
  smoking' <- return smoking_int
  ...
  return ((stress,smoking,cancer),(stress,smoking',cancer'))
```

Note that if `(a,a') <- individual_model' p s` are two individuals, then `a` and `a'` are **not** independent, because e.g. `stress` is shared between them. That's the whole point of counterfactuals: world are correlated because of exogenous variables.  

We can now define

```haskell
population_model' : Params -> P ([Individual], [Individual])
```

where people are randomly either smoking or not. There are two ways of doing that

1. run `individual_model' p True` and `individual_model' p False` for exactly `n_individuals/2` many iterations.
2. for `n_individuals` many iterations, sample `s <- flip(0.5)` fairly and run `individual_model' p s`.

In either case, we concat all the results. In the ChiRho tutorial, they chose approach #1. It has less variance, and is also what we would get in a control trial were we assign exactly 5000 people to each test group. It also makes computing the empirical ATE easier.

## Counterfactual Inference

Take synthetic data `data` as before. We again put a prior on `params`, sample population and counterfactual population, and condition on the data.

```haskell
params <- prior
(pop, pop') <- population_model' params
condition(pop == data)
```

We have now two different approaches for how to get a prediction.

> Approach 1: Return `pop'`

```haskell
params <- prior
(pop, pop') <- population_model' params
condition(pop == data)
return pop'
```

> Approach 2: Train parameters, and return a fresh counterfactual sample

This would be

```haskell
params <- prior
(pop, _) <- population_model' params
condition(pop == data*)
(_, pop') <- population_model' params
return pop'
```

Note that this is equivalent to
```haskell
params <- prior
pop <- population_model params
condition(pop == data*)
(_, pop') <- population_model' params
return pop'
```

and in turn equivalent to
```haskell
params <- posterior
(_, pop') <- population_model' params
return pop'
```

In this approach, inference only uses the factual model, finds a good parameter, and plugs that into the counterfactual model.

**Question:** Which one do we want to be doing? Does the difference matter?

I believe the variational inference example lends itself to Approach 2, but I'm not sure. 

By the way, if the two outputs of `population_model'` *were independent*, then there would be no difference between the two approaches. But we've seen that they are not, because exogenous variables are shared between them.

# Empirical ATE

In the ChiRho tutorial, what we are interested in numerically (and use in plots) is the empirical average treatment effect. This is a quantity that can be numerically computed from a population `pop : [Individual]`:

* Given such a population, we split it into two group: smokers and non-smokers.
* We compute the proportions `cancer_among_smokers` and `cancer_among_nonsmokers`
* We return `ate = cancer_among_smokers - cancer_among_nonsmokers`.

This defines a function `ate : [Individual] -> Double`. 

Clearly, `ate` involves a calculation of empirical conditional probabilities from a population. This calculation can fail if we get really degenerate populations, e.g. everybody smokes. That's why it's good to assign exactly 5000 individuals in each group.

Given a distribution `individual_dist : P Individual` we can mathematically define an exact ATE as the difference between conditional probabilities. 
```haskell
exact_ate : P Individual -> Double
```

The `ate` function is then just a simple numerical calculation of

```haskell
ate pop = exact_ate (sample_uniformly pop)
```

The plot of ATE in the tutorial is the distribution

```haskell
(pop,pop') <- population_model' params
return (ate pop')
```

Its variance comes from the fact that the population is of finite size. For `n_individuals -> infty`, this approaches the exact value of

```haskell
exact_ate $ do
    smoke_int <- flip 0.5
    (x, x') <- individual_model' params smoke_int
    return x'
```

# Full workflow

Here is the inference problem we want to run in the end. 

```haskell
params <- prior
(pop, _) <- population_model' params
condition(pop == data*)
(_, pop') <- population_model' params
return (ate pop')
```