{-
FLP 2022
Variant : DKA-2-MKA
Author  : Jan Krejci
Login   : xkrejc70
-}

module Rename (
    myPrint
) where

import Types
import Data.List (sort)

-- print DFA/MKA in required output format
-- rename states
myPrint :: DFA -> IO ()
myPrint dfa' = do
    let toProcess = processStatesOrder dfa'
    -- print toProcess
    let dfa'' = renameDFA dfa' toProcess
    print dfa''

-- order states in transitions in required format by reachability from init state
-- sort transitions so next state in succ transition will be max increment of previous one
processStatesOrder :: DFA -> [State]
processStatesOrder dfa' = do
    let toProcess = getFirstStates dfa'
    getStatesOrder (sort $ transitions dfa') toProcess []

-- states are ordered by reachability
getStatesOrder :: [Transition] -> [State] -> [State] -> [State]
getStatesOrder _ [] statesOrder = statesOrder
getStatesOrder trans toProcess statesOrder = do
    let toProcess' = getProcessStates toProcess statesOrder (sort trans) ++ (init toProcess)
    getStatesOrder trans toProcess' (last toProcess : statesOrder)

-- get next state in transitions of current proccessing state
getProcessStates :: [State] -> [State] -> [Transition] -> [State]
getProcessStates _ _ [] = []
getProcessStates toProcess statesOrder (tran:trans) = if cs tran == last toProcess
    then if ns tran `elem` toProcess || ns tran `elem` statesOrder
        then getProcessStates toProcess statesOrder trans
        else getProcessStates ((ns tran) : toProcess) statesOrder trans ++ [ns tran]
    else getProcessStates toProcess statesOrder trans

-- get next state of first init transition (lexicographic order)
getFirstStates :: DFA -> [State]
getFirstStates dfa' = do
    -- init state first
    let statesOrder = [initState dfa']
    let ns'' = ns (minimum $ getInitTransition (initState dfa') (transitions dfa'))
    if ns'' == initState dfa'
        then statesOrder
        else ns'' : statesOrder

getInitTransition :: State -> [Transition] -> [Transition]
getInitTransition initS = foldr (\tran acc -> if cs tran == initS then tran : acc else acc) []

renameDFA :: DFA -> [State] -> DFA
renameDFA dfa' statesOrder = do
    let states' = [0..toInteger $ length statesOrder - 1]
    let initS = 0
    let finalS = renameFinalStates (finalStates dfa') statesOrder 0 []
    let trans = renameAllTransitions (toRenamedTrans $ transitions dfa') statesOrder 0
    updateDFA dfa' states' initS finalS (toTrans trans)

renameFinalStates :: [State] -> [State] -> Integer -> [State] -> [State]
renameFinalStates _ [] _ newFinalS = newFinalS
renameFinalStates finalS statesOrder n newFinalS = if last statesOrder `elem` finalS
    then renameFinalStates finalS (init statesOrder) (n+1) (n : newFinalS)
    else renameFinalStates finalS (init statesOrder) (n+1) newFinalS

renameAllTransitions :: [RenamedTransition] -> [State] -> Integer -> [RenamedTransition]
renameAllTransitions trans [] _ = trans
renameAllTransitions trans statesOrder n = do
    let renamedTrans = renameTransitions trans (last statesOrder) n []
    renameAllTransitions renamedTrans (init statesOrder) (n+1)

-- rename only not renamed transition states
-- example: rename state 1
renameTransitions :: [RenamedTransition] -> State -> Integer -> [RenamedTransition] -> [RenamedTransition]
renameTransitions [] _ _ newTrans = newTrans
renameTransitions (tran:trans) st n newTrans
    -- transition 1a1
    | (fst (cs' tran) == st) && (fst (ns' tran) == st) = do
        -- rename only not renamed states
        if (notRenamed (cs' tran)) && (notRenamed (ns' tran))
            then renameTransitions trans st n (RenamedTransition (n,True) (is' tran) (n,True) : newTrans)
            else if (notRenamed (cs' tran)) && (not $ notRenamed (ns' tran))
                then renameTransitions trans st n (RenamedTransition (n,True) (is' tran) (ns' tran) : newTrans)
                else if (not $ notRenamed (cs' tran)) && (notRenamed (ns' tran))
                    then renameTransitions trans st n (RenamedTransition (cs' tran) (is' tran) (n,True) : newTrans)
                    else renameTransitions trans st n (RenamedTransition (cs' tran) (is' tran) (ns' tran) : newTrans)

    -- transition 1a_ (rename current state)
    | fst (cs' tran) == st && notRenamed (cs' tran) =
        renameTransitions trans st n (RenamedTransition (n,True) (is' tran) (ns' tran) : newTrans)

    -- transition _a1 (rename next state)
    | (fst (cs' tran) /= st) && (fst (ns' tran) == st && notRenamed (ns' tran)) =
        renameTransitions trans st n (RenamedTransition (cs' tran) (is' tran) (n,True) : newTrans)

    -- 1 is not in transition
    | otherwise =
        renameTransitions trans st n (tran : newTrans)


notRenamed :: (State, Bool) -> Bool
notRenamed (_, renamed) = not renamed