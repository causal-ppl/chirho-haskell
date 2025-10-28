# Bob's car

Consider Bob's car example

```haskell
bobCarModel fuelKey = do 
    fuel <- sample (pure (bernoulli 0.8));
    fuelInt <- new_ fuelKey fuel;
    battery <- sample (pure (bernoulli 0.9));
    let carStarts = ((&&) <$> fuelInt) <*> battery;
    return (fuelInt, battery, carStarts);
    
bobCarModelIntervenedAndConditioned fuelKey = do 
    let intervenedModel = do_ fuelKey (Value True) "fuelTrue" (bobCarModel fuelKey)
    (fuelInt, battery, carStarts) <- intervenedModel;
    conditionFactual not carStarts;
    return (fuelInt, battery, carStarts);

main = do
    fuelKey <- createKey
    getM (bobCarModelIntervenedAndConditioned fuelKey) empty 
```

We step through an example execution:

* We generate some `fuelKey`, say `#1`. Because our code uses a state-passing translation, it gets passed around everywhere. Instead, imagine we just substitute this value in everywhere and now we're dealing with the following program

```haskell
bobCarModel = do 
    fuel <- sample (pure (bernoulli 0.8));
    fuelInt <- new_ #1 fuel;
    battery <- sample (pure (bernoulli 0.9));
    let carStarts = ((&&) <$> fuelInt) <*> battery;
    return (fuelInt, battery, carStarts);
    
bobCarModelIntervenedAndConditioned = do 
    let intervenedModel = do_ #1 (Value True) "fuelTrue" bobCarModel
    (fuelInt, battery, carStarts) <- intervenedModel;
    conditionFactual not carStarts;
    return (fuelInt, battery, carStarts);

main = getM bobCarModelIntervenedAndConditioned empty 
```
* `bobCarModelIntervenedAndConditioned` is a causal computation. Causality is modelled as a reader monad for an **environment** of **intervention instructions**. The two commands that interacts with the environment is `new_` (reading from the environment) and `do_` (running a computation under a modified environment).

* So in `main`, we begin running `bobCarModelIntervenedAndConditioned` in the empty environment (no intervention instructions).
* To execute `bobCarModelIntervenedAndConditioned`, we run `intervenedModel`: That is just like `bobCarModel` except the `do_` handler is wrapped around it. `do_` runs the computation `bobCarModel` under the modified environment ``empty`` plus an instruction saying that intervention point #1 should be intervened on with a named branching, to the effect of `(Value True)`, i.e. returning the value True.

* So we step into `bobCarModel`:

We use tables and tracked values interchangeably.

```haskell
fuel <- sample (pure (bernoulli 0.8));
```

This takes the distribution object ``bernoulli 0.8``, puts it in a pure table `([], { []: bernoulli 0.8 })` and then uses `sample` to sample (memoize) each component of the table independently. After that, we get a table 

```
fuel = ([], { []: p })
```

where `p ~ bernoulli 0.8` is some boolean sample.

Now we need to interpret `new_ #1 fuel`.

* This checks the environment: It finds an intervention instruction pertaining to intervention point #1 with name `fuelTrue` and operation `Value True`
* So it takes the table `fuel` and adds a new branching name `fuelTrue` to it. We now need to update the table
* If `fuelTrue=false`, this is the "real branch", so we keep a copy of the old table
* If `fuelTrue=true`, we perform the intervention, meaning we plug in the value `True`. So we get

```haskell
fuelInt = (["fuelTrue"], { [false]: p, [true]: True })
```

The line
```haskell
battery <- sample (pure (bernoulli 0.9))
```
is as before; it sets
```
battery = ([], { []: q })
```
where `q ~ bernoulli(0.9)`.

Now in 
```haskell
let carStarts = ((&&) <$> fuelInt) <*> battery;
```
we mash the tables together using broadcasting. We have
```
carStarts = (["fuelTrue"], { [false]: p && q, [true]: True && q })
```

Now, we're back to `bobCarModelIntervenedAndConditioned`:

```haskell
conditionFactual not carStarts
```

This is the same as extracting the factual value of `carStarts`, and conditioning it to be false. Factual value means indexing at the world where all branchings are assigned false (factual):

```
condition(p && q == false)
```

Finally, we should want to extract the counterfactual value `carStarts[allTrue]`.

Here is the fully expanded execution

```haskell
p <- bernoulli(0.8)
fuel = ([], { []: p })
fuelInt = (["fuelTrue"], { [false]: p, [true]: True })
q <- bernoulli (0.9)
battery = ([], { []: q })
carStarts = (["fuelTrue"], { [false]: p && q, [true]: True && q })

observe(carStarts[[false]] = false)
return carStarts[[true]]
```

**Observations**
* keys are intervention points
* names identify branchings
* a world is a specification of which branchings we took  
* we can create zero, one, or more branchings at intervention points, by supplying an intervention (and a name for that branching)

# Alice's car

In Alice's car, we want electronics to be independently sampled from the branches

The code for Alices' car is

```haskell
aliceCarModel fuelKey = do -- Caus
    fuel <- sample (pure (bernoulli 0.8));
    fuelInt <- new_ fuelKey fuel;
    let electronicNoise f = do -- Caus
            electronics <- bernoulli 0.9;
            return $ f && electronics;
    carStarts <- sample (electronicNoise <$> fuelInt)
    return (fuelInt, carStarts)
```

The run begins in the same way as 
```haskell
p <- bernoulli(0.8)
fuel = ([], { []: p })
fuelInt = (["fuelTrue"], { [false]: p, [true]: True })
```
Now we compute `electronicNoise <$> fuelInt`, which is the following table of probability distributions!

```haskell
(["fuelTrue"], { [false]: electronicNoise p, [true]: electronicNoise True })
```

Sampling gives
```haskell
s1 <- electronicNoise p
s2 <- electronicNoise True
carStarts = (["fuelTrue"], { [false]: s1, [true]: s2 })
```

that is the samples in different worlds are *independent*.
