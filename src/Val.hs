{-# LANGUAGE InstanceSigs #-}
module Val where

import Control.Monad.Bayes.Class
import Data.List (sort, sortBy)
import Data.Map (Map, empty, fromList, fromSet, insert, lookup, restrictKeys, toList, union)
import Data.Set (Set, empty, fromList, insert, toList, union)
import Common
    ( NameSet(..),
      Name,
      emptyNameSet,
      insertName,
      unionNameSet,
      fromListNameSet )

-- | This module defines the Val type and related functions.

-- Everything from here onwards is for the Val applicative

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

newtype Val a = Val (NameSet, World -> a)

getVal :: Val a -> (NameSet, World -> a)
getVal (Val v) = v

-- sum_(n \subseteq Names) (2^n -> a)
-- sum_(n \subseteq Names) (f : 2^Names -> a | totally defined by values at n)
-- Every (Val a) will have a dependency set and World -> a function that is *constant* for any world w the same value of NameSet
-- In other words, we have the restriction operation built in. Could also make the restriction operation explicit, and
-- reimplement the various operations in terms of it.

-- N \subseteq Names |-> [2^N]
getAllRelevantWorlds :: NameSet -> [World]
getAllRelevantWorlds (NS set) = foldr (\n ws -> concatMap (\w -> [insertWorld n False w, insertWorld n True w]) ws) [emptyWorld] set

-- Utilities
instance (Show a) => Show (Val a) where
  show :: Val a -> String
  show (Val (ns, f)) =
    let relevantWorlds = getAllRelevantWorlds ns
     in "Val { ns: "
          ++ show ns
          ++ ", f: ["
          ++ concatMap (\(worldStr, val) -> worldStr ++ ": " ++ show val ++ ", ") (zip (map show relevantWorlds) (map f relevantWorlds))
          ++ "] }"

-- TODO: Make a better printing mechanism

instance (Eq a) => Eq (Val a) where
  (Val (ns1, f)) == (Val (ns2, g)) =
    (ns1 == ns2)
      && let relevantWorlds = getAllRelevantWorlds ns1
          in all (\w -> f w == g w) relevantWorlds

instance (Ord a) => Ord (Val a) where
  -- This is a completely arbitrary ordering but it works for now.
  compare :: Val a -> Val a -> Ordering
  (Val (ns1, f)) `compare` (Val (ns2, g)) =
    case compare ns1 ns2 of
      EQ ->
        let relevantWorlds = getAllRelevantWorlds ns1
         in foldr (\w acc -> if f w == g w then acc else compare (f w) (g w)) EQ relevantWorlds
      ord -> ord

-- Definition starts from here.
instance Functor Val where
  fmap :: (a -> b) -> Val a -> Val b
  fmap f (Val (ns, w)) = Val (ns, f . w)

instance Applicative Val where
  pure x = Val (emptyNameSet, const x)
  (<*>) :: Val (a -> b) -> Val a -> Val b
  Val (ns1, f) <*> Val (ns2, g) = Val (unionNameSet ns1 ns2, \w -> f w (g w)) -- Restrict keys already implicit

-- Distributive Law

-- Restrict to the name set `ns`, and then pad with false
padRestrict :: NameSet -> World -> World
padRestrict ns w = restrictWorld ns w `unionWorld` fromSetWorld (const False) ns

-- Only works well for commutative monads
memDist :: (Monad t) => Val (t a) -> t (Val a)
memDist (Val (ns, f)) =
  let accFunction =
        foldr
          ( \world acc ->
              do g <- acc; value <- f world; return (\w -> if w == world then value else g w) -- Crazy inefficient
          )
          (pure (const $ error "Should never occur")) -- Lack dependent types
          $ getAllRelevantWorlds ns
   in do acc <- accFunction; return (Val (ns, acc . padRestrict ns)) -- Key restriction happens here.

sample :: (Monad t) => Val (t a) -> t (Val a)
sample = memDist

data Intervention t a = None | Value a | Func (a -> t a)

intervene :: (Monad t) => Val a -> Intervention t a -> Name -> t (Val a)
intervene (Val (ns, f)) None _ = return (Val (ns, f)) -- No change
intervene (Val (ns, f)) (Value v) n =
  let newNameSet = insertName n ns
   in return
        ( Val
            ( newNameSet,
              \w -> case lookupWorld n w of
                Just True -> v
                _ -> f w
            )
        )
intervene (Val (ns, f)) (Func g) n =
  let newNameSet = insertName n ns
   in memDist
        ( Val
            ( newNameSet,
              \w -> case lookupWorld n w of
                Just True -> return (f w)
                _ -> g (f w)
            )
        )

liftOp :: (Monad t) => (a -> t b) -> Val a -> t (Val b)
liftOp f = memDist . fmap f

-- Utilities for Score and Condition
scoreAll :: (MonadFactor m) => Val (Log Double) -> m ()
scoreAll v = do
  _ <- liftOp score v
  return ()

scoreFactual :: (MonadFactor m) => (a -> Log Double) -> Val a -> m ()
scoreFactual f (Val (ns, w)) = score (f (w $ constWorld False ns))


scoreCounterFactual :: (MonadFactor m) => (a -> Log Double) -> Val a -> m ()
scoreCounterFactual f (Val (ns, w)) = score (f (w $ constWorld True ns))

conditionAll :: (MonadFactor m) => Val Bool -> m ()
conditionAll v = do
  _ <- liftOp condition v
  return ()

conditionFactual :: (MonadFactor m) => (a -> Bool) -> Val a -> m ()
conditionFactual f (Val (ns, w)) = condition (f (w (constWorld False ns)))

conditionCounterFactual :: (MonadFactor m) => (a -> Bool) -> Val a -> m ()
conditionCounterFactual f (Val (ns, w)) = condition (f (w $ constWorld True ns))

-- End Val applicative

-- do_ :: (Monad m) => (World -> World) -> m a -> m a
-- do_ f 
