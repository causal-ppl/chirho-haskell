{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main (main) where
import Control.Monad.Bayes.Class
import Chirho
import Control.Monad.Bayes.Sampler.Strict
import Data.HMap
import Control.Monad.Bayes.Inference.Lazy.MH (mh)

import qualified Models.AliceBob 
import qualified Models.Smoking 


main :: IO ()
main = Models.AliceBob.main