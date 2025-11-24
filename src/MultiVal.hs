{-# LANGUAGE InstanceSigs #-}
module MultiVal where

import Control.Monad.Bayes.Class
import Data.List (sort, sortBy)
import Data.Map (Map, empty, fromList, fromSet, insert, lookup, restrictKeys, toList, union)
import Data.Set (Set, empty, fromList, insert, toList, union)
import Name
    ( NameSet(..),
      Name,
      emptyNameSet,
      insertName,
      unionNameSet,
      fromListNameSet )

-- | This module defines the MultiVal type and related functions.

-- Everything from here onwards is for the MultiVal applicative

newtype World = W (Map Name Bool) deriving (Eq, Ord)

instance Show World where
  show :: World -> String
  show (W m) =
    let sorted = sortBy (\(a, _) (b, _) -> a `compare` b) $ Data.Map.toList m
     in "[" ++ concatMap (\(_, b) -> if b then "1" else "0") sorted ++ "]"

emptyWorld :: World
emptyWorld = W Data.Map.empty

insertWorld :: Name -> Bool -> World -> World
insertWorld n b (W m) = W (Data.Map.insert n b m)

restrictWorld :: NameSet -> World -> World
restrictWorld (NS set) (W m) = W (Data.Map.restrictKeys m set)

fromSetWorld :: (Name -> Bool) -> NameSet -> World
fromSetWorld f (NS set) = W (Data.Map.fromSet f set)

unionWorld :: World -> World -> World
unionWorld (W m1) (W m2) = W (Data.Map.union m1 m2)

lookupWorld :: Name -> World -> Maybe Bool
lookupWorld n (W m) = Data.Map.lookup n m

fromListWorld :: [(Name, Bool)] -> World
fromListWorld xs = W (Data.Map.fromList xs)

constWorld :: Bool -> NameSet -> World
constWorld b (NS set) = W (Data.Map.fromSet (const b) set)

newtype MultiVal a = MultiVal (NameSet, World -> a)
-- MultiVal a represents a value that can depend on multiple "names" (branching points)
-- \sum_(n \subseteq Names) (2^n -> a)
-- \sum_(n \subseteq Names) (f : 2^Names -> a | totally defined by Values at n)
-- Every (MultiVal a) will have a dependency set and World -> a function that is *constant* for any world w the same Value of NameSet
-- In other words, we have the restriction operation built in. Could also make the restriction operation explicit, and
-- reimplement the various operations in terms of it.

getMultiVal :: MultiVal a -> (NameSet, World -> a)
getMultiVal (MultiVal v) = v

-- N \subseteq Names |-> [2^N]
getAllRelevantWorlds :: NameSet -> [World]
getAllRelevantWorlds (NS set) = foldr (\n ws -> concatMap (\w -> [insertWorld n False w, insertWorld n True w]) ws) [emptyWorld] set

-- Utilities

getFactual :: MultiVal a -> a
getFactual (MultiVal (ns, f)) = f (constWorld False ns)

getCounterfactual :: MultiVal a -> a
getCounterfactual (MultiVal (ns, f)) = f (constWorld True ns)

instance (Show a) => Show (MultiVal a) where
  show :: MultiVal a -> String
  show (MultiVal (ns, f)) =
    let relevantWorlds = getAllRelevantWorlds ns
     in "MultiVal { ns: "
          ++ show ns
          ++ ", f: ["
          ++ concatMap (\(worldStr, val) -> worldStr ++ ": " ++ show val ++ ", ") (zip (map show relevantWorlds) (map f relevantWorlds))
          ++ "] }"

-- TODO: Make a better printing mechanism

instance (Eq a) => Eq (MultiVal a) where
  (MultiVal (ns1, f)) == (MultiVal (ns2, g)) =
    (ns1 == ns2)
      && let relevantWorlds = getAllRelevantWorlds ns1
          in all (\w -> f w == g w) relevantWorlds

instance (Ord a) => Ord (MultiVal a) where
  -- This is a completely arbitrary ordering but it works for now.
  compare :: MultiVal a -> MultiVal a -> Ordering
  (MultiVal (ns1, f)) `compare` (MultiVal (ns2, g)) =
    case compare ns1 ns2 of
      EQ ->
        let relevantWorlds = getAllRelevantWorlds ns1
         in foldr (\w acc -> if f w == g w then acc else compare (f w) (g w)) EQ relevantWorlds
      ord -> ord

instance Functor MultiVal where
  fmap :: (a -> b) -> MultiVal a -> MultiVal b
  fmap f (MultiVal (ns, w)) = MultiVal (ns, f . w)

instance Applicative MultiVal where
  pure x = MultiVal (emptyNameSet, const x)
  (<*>) :: MultiVal (a -> b) -> MultiVal a -> MultiVal b
  MultiVal (ns1, f) <*> MultiVal (ns2, g) = MultiVal (unionNameSet ns1 ns2, \w -> f w (g w)) -- Restrict keys already implicit

-- Distributive Law

-- Restrict to the name set `ns`, and then pad with false
padRestrict :: NameSet -> World -> World
padRestrict ns w = restrictWorld ns w `unionWorld` fromSetWorld (const False) ns

-- Only works well for commutative monads
memDist :: (Monad t) => MultiVal (t a) -> t (MultiVal a)
memDist (MultiVal (ns, f)) =
  let accFunction =
        foldr
          ( \world acc ->
              do g <- acc; val <- f world; return (\w -> if w == world then val else g w) -- Crazy inefficient
          )
          (pure (const $ error "Should never occur")) -- Lack dependent types
          $ getAllRelevantWorlds ns
   in do acc <- accFunction; return (MultiVal (ns, acc . padRestrict ns)) -- Key restriction happens here.

sample :: (Monad t) => MultiVal (t a) -> t (MultiVal a)
sample = memDist

data Intervention t a = None | Value a | Func (a -> t a)

intervene :: (Monad t) => MultiVal a -> Intervention t a -> Name -> t (MultiVal a)
intervene (MultiVal (ns, f)) (Func g) n =
  let newNameSet = insertName n ns
   in memDist
        ( MultiVal
            ( newNameSet,
              \w -> case lookupWorld n w of
                Just True -> g (f w)
                -- Just True -> let fw = insertWorld n False w in g (f fw)
                _ -> return (f w) 
            )
        )
intervene (MultiVal (ns, f)) None _ = return (MultiVal (ns, f)) -- No change
intervene (MultiVal (ns, f)) (Value v) n =
  let newNameSet = insertName n ns
   in return
        ( MultiVal
            ( newNameSet,
              \w -> case lookupWorld n w of
                Just True -> v
                _ -> f w
            )
        )



liftOp :: (Monad t) => (a -> t b) -> MultiVal a -> t (MultiVal b)
liftOp f = memDist . fmap f

-- Utilities for Score and Condition
scoreAll :: (MonadFactor m) => MultiVal (Log Double) -> m ()
scoreAll v = do
  _ <- liftOp score v
  return ()

scoreFactual :: (MonadFactor m) => (a -> Log Double) -> MultiVal a -> m ()
scoreFactual f (MultiVal (ns, w)) = score (f (w $ constWorld False ns))


scoreCounterFactual :: (MonadFactor m) => (a -> Log Double) -> MultiVal a -> m ()
scoreCounterFactual f (MultiVal (ns, w)) = score (f (w $ constWorld True ns))

conditionAll :: (MonadFactor m) => MultiVal Bool -> m ()
conditionAll v = do
  _ <- liftOp condition v
  return ()

conditionFactual :: (MonadFactor m) => (a -> Bool) -> MultiVal a -> m ()
conditionFactual f (MultiVal (ns, w)) = condition (f (w (constWorld False ns)))

conditionCounterFactual :: (MonadFactor m) => (a -> Bool) -> MultiVal a -> m ()
conditionCounterFactual f (MultiVal (ns, w)) = condition (f (w $ constWorld True ns))

-- End MultiVal applicative

-- do_ :: (Monad m) => (World -> World) -> m a -> m a
-- do_ f 
