{-
FLP 2022
Variant : DKA-2-MKA
Author  : Jan Krejci
Login   : xkrejc70
-}

module Types where

import Data.List (intercalate, sort)

type State = Integer
type FinalStates = [State]
type Symbol = Char

-- deterministic finite automaton (DFA)
data DFA = DFA {
    states :: [State],
    alphabet :: [Symbol],
    initState :: State,
    finalStates :: FinalStates,
    transitions :: [Transition]
} deriving (Eq)

-- state-transition
data Transition = Transition {
    cs :: State,    -- current state
    is :: Symbol,   -- input symbol
    ns :: State     -- next state
} deriving (Eq, Ord)

-- renamed state-transition
data RenamedTransition = RenamedTransition {
    cs' :: (State, Bool),   -- renamed current state
    is' :: Symbol,          -- input symbol
    ns' :: (State, Bool)    -- renamed next state
} deriving (Show, Eq, Ord)

-- show DFA in required output format
instance Show DFA where
    show (DFA states' alphabet' initState' finalStates' transitions') =
        prepareStates (sort states') ++ "\n" ++
        sort alphabet' ++ "\n" ++
        s initState' ++ "\n" ++
        prepareStates (sort finalStates') ++ "\n" ++
        prepareTransitions (sort transitions')
            where s x = show x

instance Show Transition where
    show (Transition current input next) =  s current ++ "," ++ [input] ++ "," ++ s next
        where s x = show x

prepareStates :: [State] -> String
prepareStates origStates = intercalate "," (map show origStates)

prepareTransitions :: [Transition] -> String
prepareTransitions trans = intercalate "\n" (map show trans)

-- program action: I or T
data Action = I | T deriving (Show, Eq)

-- stores program acion input (DFA type)
data Result = Result {
    action :: Action,
    dfa :: IO String
}

-- groups used for minimization (equivalence classes)
data Groups = Groups {
    changed :: Bool,
    oldGroup :: [State],
    newGroups :: [[State]]
}

-- DFA setters
setStates :: DFA -> [State] -> DFA
setStates dfa' newStates = dfa' {states = newStates}

setInitState :: DFA -> State -> DFA
setInitState dfa' newInitState = dfa' {initState = newInitState}

setFinalStates :: DFA -> [State] -> DFA
setFinalStates dfa' newFinalStates = dfa' {finalStates = newFinalStates}

setTransitions :: DFA -> [Transition] -> DFA
setTransitions dfa' newTransitions = dfa' {transitions = newTransitions}

-- Transition setters
setCurrentState :: Transition -> State -> Transition
setCurrentState tran newCurrentState = tran {cs = newCurrentState}

setNextState :: Transition -> State -> Transition
setNextState tran newNextState = tran {ns = newNextState}

-- update DFA
updateDFA :: DFA -> [State] -> State -> [State] -> [Transition] -> DFA
updateDFA dfa' reachS initS finalS trans = do
    let dfa1 = setStates dfa' (sort reachS)
    let dfa2 = setInitState dfa1 initS
    let dfa3 = setFinalStates dfa2 (sort finalS)
    let dfa4 = setTransitions dfa3 (sort trans)
    dfa4

-- convert transitions to renamed transitions
toRenamedTrans :: [Transition] -> [RenamedTransition]
toRenamedTrans = map (\tran -> RenamedTransition (cs tran, False) (is tran) (ns tran, False))

-- convert renamed transitions to transitions
toTrans :: [RenamedTransition] -> [Transition]
toTrans = map (\tran -> Transition (fst (cs' tran)) (is' tran) (fst (ns' tran)))
