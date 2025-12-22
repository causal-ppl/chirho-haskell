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

main :: IO ()
main = do
    testModelMain
    -- Placeholder for mediation analysis code
    print "Mediation Analysis module executed."
