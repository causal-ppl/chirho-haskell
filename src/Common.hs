{-# LANGUAGE InstanceSigs #-}
module Common where
import Data.Set
import Data.List (sort)

-- Common utility functions and types can be defined here
type Name = String

newtype NameSet = NS (Set Name) deriving (Eq, Ord)

emptyNameSet :: NameSet
emptyNameSet = NS Data.Set.empty

insertName :: Name -> NameSet -> NameSet
insertName n (NS set) = NS (Data.Set.insert n set)

unionNameSet :: NameSet -> NameSet -> NameSet
unionNameSet (NS set1) (NS set2) = NS (Data.Set.union set1 set2)

fromListNameSet :: [Name] -> NameSet
fromListNameSet names = NS (Data.Set.fromList names)

instance Show NameSet where
  show :: NameSet -> String
  show (NS set) = "{" ++ concatMap (++ ", ") (sort (Data.Set.toList set)) ++ "}"

