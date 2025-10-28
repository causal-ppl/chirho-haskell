# A typed implementation of [ChiRho](https://basisresearch.github.io/chirho/index.html) in Haskell

ChiRho-haskell is a causal probabilistic programming library. Its programming paradigm is extracted from the [ChiRho library](https://basisresearch.github.io/chirho/index.html) in Python. The aim of this implementation is to demonstrate the abstractions we developed to model ChiRho's programming paradigm, and how they enable type-safe *and* automatic (Bayesian) causal modelling and inference, where intervention and counterfactuals are supported as first-class constructs. 

ChiRho was originally implemented in Python on top of [Pyro](https://pyro.ai). ChiRho-haskell is built on top of [`MonadBayes`](https://monad-bayes.netlify.app), a popular bayesian probabilistic programming library in Haskell. 

## Hacking

### Setup
<!-- Install Haskell, Install matplotlib and python -->
This system uses `stack`. Please [install](https://docs.haskellstack.org/en/v1.1.2/install_and_upgrade/) `stack` to use it in the intended way.

To build, simply run the following. If it is your first time building with `stack`, it might take a while for it to install the required environment and packages. 
```sh
stack build
```
For the purpose of plotting, also install the Python packages for `matplotlib, numpy, tk, scipy`. To do so, simply run
```sh
python3 -m pip install -U matplotlib numpy tk scipy
```

Finally, to execute a program, e.g. one of our examples, `alicebob`, simply run:
```sh
stack exec alicebob
```

### Writing your own causal model?
To write your own causal model, just create a new file, say `MyOwnModel.hs` under `app/`, and add in `package.yaml` the following before building with `stack`. 
```yaml
  myownmodel:
    main:                MyOwnModel.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    - -main-is MyOwnModel
    dependencies:
    - chirho
```
Then, use `stack exec myownmodel` to execute your model. 

## Examples

We have implemented three sets of examples.
- `alicebob` (in `AliceBob.hs`): contains a simple scenario of doing inference on the causal model of whether a car will start. The Bob model is the one presented in the extended abstract: the car starts if both it has fuel and its battery is not dead, which happen at a certain probability. Given that it did not actually start, what's the probability that it would have started had Bob refueled his car the day before. Note that both of `fuel` and `battery` are treated as exogenous: if we intervene on fuel, the factual world and the counterfactual world will share the same battery sample. The Alice task is similar, except the battery fault (renamed as `electronics`) occurs endogenously, i.e. in the factual and counterfactual worlds, the fault happens independently. This example demonstrates how both can be implemented, and some subtlety about how the applicative functor and the monad interact. More details [here](/notes/AliceBob.md).
- `keysnames` (in `KeysNames.hs`): contains examples showing why keys and names should be different, and what expressivity gain we obtain by keeping them distinct. In particular, we show why it might be useful to have interventions on the same intervention point and different names, as well as having interventions on different intervention points with the same name. More details [here](/notes/KeysNames.md).
- `smoking` (in `Smoking.hs`): this is a (currently partial) reproduction of the [ChiRho tutorial](https://basisresearch.github.io/chirho/tutorial_i.html).

## Library
- `ChiRho.hs`: the entry point to the library. This module imports all other modules and specifies the identifiers to be exported.
- Interventional mechanisms:
  - `InterventionEnv.hs`: Implementation of the intervention environment, which is a heterogenous map (in the programming sense) associating `InterventionPointKey`'s to the intervention instructions of the corresponding type, to be executed at that intervention point. Under the hood, `InterventionEnv` is implemented as an `HMap`, and this `InterventionEnv` encapsulates it. Unfortunately, the fact that we use `HMap` means that the intervention point keys will have to be generated at run time. 
  - `CausMonad.hs`: Implementation of the `Caus a ~= InterventionEnv -> Prob a` monad, which encapsulates the interventional mechanisms. This monad enables two operations:
    - `new_: (Monad m) => InterventionPointKey m a -> MultiVal a -> Caus m (MultiVal a)` which creates a new intervention point in the program keyed by its first argument,
    - `do_ :: InterventionPointKey m a -> Intervention m a -> Name -> Caus m b -> Caus m b` which inserts an intervention instruction at the intervention point keyed by the first argument. 
- Counterfactual mechanisms:
  - `MultiVal.hs`: Implementation of the `MultiVal a` applicative, used (as explained) to perform twinning automatically. 
    - A value in `MultiVal a` can informally be thought of as a tuple `(N, f)`, where `N` is a finite subset of names, and `f` is a function which for every bitvector `N -> Bool`, gives a value of type `a`. The idea is that each of these names can be thought of as a *binary branching point* in the program, generating different *worlds* defined by the branches taken (the bitvector of type `N -> Bool`, equivalently a subset of `N`). A value `(N, f)` of type `MultiVal a` is a *multiworld value* takes a potentially different value for each of these worlds. 
    - A multiworld value does not give a value for all possible worlds. Rather, it only gives values for each world generated by the branching points it is *supported* on, i.e. those determined by its first component. If a value is not supported on some name `b1`, it means that it is the same, or, *shared*, across the branching point. For instance, a multiworld value `({}, [{} -> 1])`  can be considered to take value `1` at world `{b1}`, at world `{b1, b2}` and so on. Multiworld values are only allowed to be different on worlds generated by the branching points they are supported on.
    - When two multiworld values are combined, the combined support is the union between the two, and the new values are simply taken from combining the old values at their respective support:
       ```haskell
        let v1 = ({b1}, [{} -> 1, {b1} -> 2]) in
        let v2 = ({b2}, [{} -> 10, {b2} -> 20]) in
            (+) <&>  v1 <*> v2 
        == ({b1, b2}, [{} -> 11, {b1} -> 12, {b2} -> 21, {b1, b2} -> 22])
       ```
       Without going into too much detail, this dependency-tracking and sharing mechanism is what corresponds to sharing non-intervened exogenous variables across the factual and counterfactual worlds when doing twinning. 
    - This interacts with the probabilistic mechanism in the following way: we have an operation `sample : MultiVal (Caus a) -> Caus (MultiVal a)` that allows multiworld distributions to be turned into a distribution over multiworld samples. This is not a distributive law: the operation does not commute with the applicative structure of `MultiVal`. We exemplify this in the [AliceBob example](/notes/AliceBob.md).
    - This interacts with the interventional mechanism in the following way: when inserting an intervention instruction, `do_` additionally requires the user to provide a name. This name is used by `do_` to generate a branching point when performing that intervention. The resulting multiworld value will now be supported on this additional name, which will distinguish worlds where the intervention has been applied, worlds where it has not. 
  - `Name.hs`: This file contains the constructs and utilities to deal with names of branching points.

## Semantic aspects
  - **Morally** the interventional mechanism (the `Caus` monad) should correspond to the following (graded) monad:
    ```math
    \mathsf{Caus} \ [X_i: A_i]_i \ B := (\prod_i \mathsf{List}(\mathsf{Intervention}(A_i) \times \mathsf{Names})) \to \mathsf{Prob}(B)
    ```
  - and the counterfactual mechanism (the `MultiVal` functor) should correspond to the following:
    ```math
    \mathsf{MultiVal}(A) := \coprod_{N \subseteq_{fin}\mathsf{Names}}[A^{(2^N)}]
    ```

## Correspondences and differences with ChiRho in Python
- `Caus` type
  - Intervention point keys: Pyro's string names on `sample` statements.
  - `do_` interventions: the `do` handler in ChiRho. Note that here there is a difference in the order in which interventions are handled: if we do two successive `do`'s the first one has precedence in ChiRho, whereas the last one has precedence in our implementation.
- `MultiVal` type: PyTorch Tensor types.
  - Branching point names: internal names to tensor dimensions, generated using handlers like     `MultiWorldCounterfactual`. 
  - Applicative structure: tensor broadcasting
  - `sample: MultiVal (Caus a) -> Caus (MultiVal a)`: vectorised sampling.

## License
This repository is licensed under the [BSD3](LICENSE) license. 
