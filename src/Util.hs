{-
FLP 2022
Variant : DKA-2-MKA
Author  : Jan Krejci
Login   : xkrejc70
-}

module Util where

import Data.List (sort, group )
import Types (State, Symbol)

-- allUnique and sq adopted from https://hackage.haskell.org/package/Unique-0.4.7.9/docs/Data-List-Unique.html
allUnique :: Ord a => [a] -> Bool
allUnique = all ( (==) 1 . length) . sg

sg :: Ord a => [a] -> [[a]]
sg = group . sort

-- cartesian product of lists
cartProd :: [State]-> [Symbol] -> [(State, Symbol)]
cartProd states symbols = [(state,symbol) | state <- states, symbol <- symbols]

-- get new unique sink state number (increment larges existing state number)
getNewSinkState :: [State] -> State
getNewSinkState states' = maximum states' + 1

-- get sink state
getSinkState :: [State] -> State
getSinkState = maximum