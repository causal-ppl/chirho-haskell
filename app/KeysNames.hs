{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module KeysNames (main) where

import Chirho
import Data.Functor.Identity

-- Dario's question on expressiveness losses if we identify keys with names

interveneOnSameKeyWithDifferentNames :: Caus m (MultiVal a) -> InterventionPointKey m Int -> Caus m (MultiVal a)
interveneOnSameKeyWithDifferentNames model key =
  let newModel1 = do_ key (Value 5) "branch1" model
   in let newModel2 = do_ key (Value 10) "branch2" newModel1
       in newModel2

interveneOnDifferentKeysWithSameName :: Caus m (MultiVal a) -> InterventionPointKey m Int -> InterventionPointKey m Int -> Caus m (MultiVal a)
interveneOnDifferentKeysWithSameName model key1 key2 =
  let newModel1 = do_ key1 (Value 5) "branch" model
   in let newModel2 = do_ key2 (Value 10) "branch" newModel1
       in newModel2

sameKeyDiffNameMain :: IO ()
sameKeyDiffNameMain = do
  print "Effect of intervening on the same key with different names:"
  key <- createKey
  print "Original model has one intervention point at default value 1."
  let model :: Caus Identity (MultiVal Int) = new_ key (pure 1)
  let model2 = interveneOnSameKeyWithDifferentNames model key
  let runModel2 = runIdentity $ getM model2 empty
  print "Expected values: [] -> 1, [branch1] -> 5, [branch2] -> 10, [branch1, branch2] -> 10 (because the second intervention on the same key takes precedence in this implementation.)"
  print "Actual values:"
  print runModel2

differentKeysSameNameMain :: IO ()
differentKeysSameNameMain = do
  print "Effect of intervening on different keys with the same name:"
  key1 <- createKey
  key2 <- createKey
  print "Original model has two intervention points both at default value 1."
  let model :: Caus Identity (MultiVal (Int, Int)) = do
        a <- new_ key1 (pure 1)
        b <- new_ key2 (pure 1)
        return $ (,) <$> a <*> b
  let model2 = interveneOnDifferentKeysWithSameName model key1 key2
  let runModel2 = runIdentity $ getM model2 empty
  print "Expected values: [] -> (1,1), [branch] -> (5,1) for key1 intervention, [branch] -> (1,10) for key2 intervention, [branch, branch] -> (5,10)."
  print "Actual values:"
  print runModel2

main :: IO ()
main = do
  sameKeyDiffNameMain
  differentKeysSameNameMain
