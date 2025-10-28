# Intervention points vs Branchings

Keys are *Intervention Points*. They refer to locations in a program where we may perform interventions. They are sort of like grades and should be tracked by the language, but here we do this with types.

Names refer to *actual branchings*.

By using `new_` and `do_`, we may create one or more actual branchings at an intervention point.

## Multiple Branchings at the same Intervention Point

```haskell
interveneOnSameKeyWithDifferentNames comp key = 
    let newModel1 = do_ key (Value 5) "branch1" comp in
    let newModel2 = do_ key (Value 10) "branch2" newModel1 in
    newModel2

```

Let `model` be a model that has boolean intervention point `key`. Running `newModel2` under some environment is like running `model` in some environment that has two additional instructions:
* Intervene at `key`, creating a branching named `branch1` which (false) leaves the value unchanged (true) sets the value to 5.
* Intervene at `key`, creating a branching named `branch2` which (false) leaves the value unchanged (true) sets the value to 10.

Take the basic model

```
model = new_ key (pure v0)
```

which simply creates an intervention point on a pure value `v0`. Running `newModel2`, we expect to see

```
(["setTrue","setFalse"], {
    [false,false] : v0
    [false,true] : 10,
    [true,false] : 5,
    [true,true] : ??
})
```

Here `??` will be either `5` or `10` depending on the order in which the implementation resolves interventions.

# Reusing Names

```haskell
interveneOnDifferentKeysWithSameName model key1 key2 = 
    let newModel1 = do_ key1 (Value 5) "counterfactual" model in
    let newModel2 = do_ key2 (Value 10) "counterfactual" newModel1 in
    newModel2
```

Here we can take a model with two intervention points, and intervene on both of them while reusing the same branching! This is actually pretty simple.
