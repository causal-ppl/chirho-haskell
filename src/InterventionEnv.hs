{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module InterventionEnv where
    
import Data.HKey
import GHC.List
import MultiVal
import Name
import Data.HMap

type InterventionInstrs m a = List (Intervention m a, Name)

newtype InterventionPointKey m a = Key (HKey T (InterventionInstrs m a))

getKey :: InterventionPointKey m a -> HKey T (InterventionInstrs m a)
getKey (Key hkey) = hkey

newtype InterventionEnv = Env HMap

getEnv :: InterventionEnv -> HMap
getEnv (Env hmap) = hmap

empty :: InterventionEnv
empty = Env Data.HMap.empty

createKey :: IO (InterventionPointKey m a)
createKey = Key <$> Data.HMap.createKey

insert :: InterventionPointKey m a -> InterventionInstrs m a -> InterventionEnv -> InterventionEnv
insert (Key k) instrs (Env hmap) =
    Env (Data.HMap.insert k instrs hmap)

findWithDefault :: InterventionInstrs m a -> InterventionPointKey m a -> InterventionEnv -> InterventionInstrs m a
findWithDefault defVal (Key k) (Env hmap) =
    Data.HMap.findWithDefault defVal k hmap
