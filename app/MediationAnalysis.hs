{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MediationAnalysis where
import Chirho
import Control.Monad.Bayes.Class
import Data.Functor.Identity


-- Test that our intervention does work as intended
testModel :: Monad m => InterventionPointKey m Int -> InterventionPointKey m Int -> InterventionPointKey m Int -> Caus m (MultiVal Int, MultiVal Int, MultiVal Int)
testModel xkey wkey ykey = do
    let x = pure 1
    xInt <- new_ xkey x
    let w = pure 10
    wInt <- new_ wkey w
    let y = ((+) <$> xInt) <*> wInt
    yInt <- new_ ykey y
    return (xInt, wInt, yInt)

testModelMain :: IO ()
testModelMain = do
    print "Mediation Analysis Test Model:"
    xkey <- createKey
    wkey <- createKey
    ykey <- createKey
    let model :: Caus Identity (MultiVal Int, MultiVal Int, MultiVal Int) = testModel xkey wkey ykey
    let pw = fromListWorld [("intervene_x", False), ("intervene_x_prime", True)]
    let intervenedModel = do_ ykey (Idx pw) "intervene_y" $ do_ xkey (Value 7) "intervene_x_prime" $ do_ xkey (Value 5) "intervene_x" $ do_ wkey (Value 20) "intervene_w" model
    let (x, w, y) = runIdentity $ run intervenedModel
    print "Original model (no interventions):"
    print y

-- TODO: Add mediation analysis code here

-- Assumption: intervene_x, intervene_x_prime, intervene_z are not used names.
naturalDirectEffect :: Num y => Monad m => Applicative collection => InterventionPointKey m x -> InterventionPointKey m z -> 
    Intervention m x -> Intervention m x -> Caus m (MultiVal (collection y)) -> Caus m (MultiVal (collection y))
naturalDirectEffect xKey zKey x xprime model =
    -- Z_x'
    let pw = fromListWorld [("intervene_x", False), ("intervene_x_prime", True)] in 
    let intervenedModel = do_ zKey (Idx pw) "intervene_z" $ do_ xKey xprime "intervene_x_prime" $ do_ xKey x "intervene_x" model in 
    do
    ys <- intervenedModel
    -- Y_x'
    let xprimePw = fromListWorld [("intervene_x", False), ("intervene_x_prime", True), ("intervene_z", False)]
    let ysXprime = lookupMultiVal xprimePw ys
    -- Y_{x,Z_x'}
    let xPw = fromListWorld [("intervene_x", True), ("intervene_x_prime", False), ("intervene_z", True)]
    let ysX = lookupMultiVal xPw ys
    return $ (\cysX cysXprime -> (-) <$> cysX <*> cysXprime) <$> ysX <*> ysXprime

testLookupMultiVal :: IO ()
testLookupMultiVal = do
    print "Testing lookupMultiVal function:"
    let x = lookupMultiVal (fromListWorld [("intervene_x", True)]) (pure 5)
    print x
    let x = lookupMultiVal (fromListWorld [("intervene_x", True)]) (fromListMultiVal [(fromListWorld [("intervene_x", True)], 10), (fromListWorld [("intervene_x", False)], 20)])
    print x
    let x = lookupMultiVal (fromListWorld [("intervene_x", False)]) (fromListMultiVal [(fromListWorld [("intervene_x", True)], 10), (fromListWorld [("intervene_x", False)], 20)])
    print x
    -- Tests with more worlds
    let x = lookupMultiVal (fromListWorld [("intervene_x", True)]) 
            (fromListMultiVal [(fromListWorld [("intervene_x", True), ("intervene_y", True)], 10), (fromListWorld [("intervene_x", True), ("intervene_y", False)], 20), (fromListWorld [("intervene_x", False), ("intervene_y", True)], 30), (fromListWorld [("intervene_x", False), ("intervene_y", False)], 40)])
    print x
    let x = lookupMultiVal (fromListWorld [("intervene_x", False), ("intervene_y", False)]) 
            (fromListMultiVal [(fromListWorld [("intervene_x", True), ("intervene_y", True)], 10), (fromListWorld [("intervene_x", True), ("intervene_y", False)], 20), (fromListWorld [("intervene_x", False), ("intervene_y", True)], 30), (fromListWorld [("intervene_x", False), ("intervene_y", False)], 40)])
    print x


main :: IO ()
main = do
    testModelMain
    testLookupMultiVal
    -- Placeholder for mediation analysis code
    print "Mediation Analysis module executed."
