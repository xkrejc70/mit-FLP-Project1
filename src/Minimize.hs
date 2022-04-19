{-
FLP 2022
Variant : DKA-2-MKA
Author  : Jan Krejci
Login   : xkrejc70
-}

module Minimize (
    minimize
) where

import Data.List (sort, union, (\\))

import Types
import Error (internalError)
import Util (cartProd, getSinkState, getNewSinkState)

-- whole process of DFA minimization
minimize :: DFA -> DFA
minimize dfa' = do
    let dfaReachableStatesOnly = removeUnreachableStates dfa'
    let dfaComplete = completeDFA dfaReachableStatesOnly
    let dfaMinimal = minimizeDFA dfaComplete
    dfaMinimal

-- ============================ Remove Unreachable States ============================

removeUnreachableStates :: DFA -> DFA
removeUnreachableStates dfa' = do
    -- initialialize
    let initReachableS = [initState dfa']
    -- get reachable states, update final states and transitions
    let reachS = findReachableStates [] initReachableS (transitions dfa')
    let initS = initState dfa'
    let finalS = getNewFinalStates reachS (finalStates dfa')
    let trans = getNewTransitions reachS (transitions dfa')
    updateDFA dfa' reachS initS finalS trans

-- recursively finds all reachable states
findReachableStates :: [State] -> [State] -> [Transition] -> [State]
findReachableStates _ reachS [] = reachS
findReachableStates prevReachS reachS trans = do
    -- until there are no new states
    if equal prevReachS reachS
        then reachS
        else do
            let newReachS = getReachableStates reachS trans
            -- add new reachable states
            let reachS' = reachS `union` newReachS
            findReachableStates reachS reachS' trans
    where
        -- true if both lists contain same elements
        equal list1 list2 = null (list1 \\ list2) && null (list2 \\ list1)

-- iterate through all the transitions
getReachableStates :: [State] -> [Transition] -> [State]
getReachableStates reachS [] = reachS
getReachableStates reachS (tran:trans) =
    if cs tran `elem` reachS
        then getReachableStates (ns tran : reachS) trans
        else getReachableStates reachS trans

getNewFinalStates :: [State] -> FinalStates -> FinalStates
getNewFinalStates newStates = foldr (\x acc -> if x `elem` newStates then x : acc else acc) []
-- why?
--getNewFinalStates finalS newStates = foldr (\x acc -> if (x `elem` newStates) then x : acc else acc) finalS []

getNewTransitions :: [State] -> [Transition] -> [Transition]
getNewTransitions newStates = foldr (\tran acc -> if cs tran `elem` newStates then tran : acc else acc) []

-- ============================ Complete ============================

completeDFA :: DFA -> DFA
completeDFA dfa' = do
    let dfa'' = addSinkState dfa'
    let allTransitions = cartProd (states dfa'') (alphabet dfa'')
    let newTransitions = completeTransitions dfa'' allTransitions
    let dfa1 = setTransitions dfa'' newTransitions
    dfa1

-- add sink state if DFA is not already complete
addSinkState :: DFA -> DFA
addSinkState dfa' = do
    let allTransitions = cartProd (states dfa') (alphabet dfa')
    if length (transitions dfa') < length allTransitions
        then setStates dfa' (getNewSinkState (states dfa') : states dfa')
        else dfa'

completeTransitions :: DFA -> [(State, Symbol)] -> [Transition]
completeTransitions dfa' = foldr (\tran acc -> if alreadyInTransitions tran (transitions dfa') then acc else uncurry Transition tran (getSinkState (states dfa')) : acc) (transitions dfa')

-- true if for specific state and symbol exists a transition
alreadyInTransitions :: (State, Symbol) -> [Transition] -> Bool
alreadyInTransitions (state, symbol) = foldr (\tran acc -> ((cs tran == state) && (is tran == symbol)) || acc) False

-- ============================ Minimize ============================

-- minimize DFA, rename states
minimizeDFA :: DFA -> DFA
minimizeDFA dfa' = do
    let groups = getGroups dfa'
    let newStates = map createNewStateName groups
    let initS = updateInitState groups (initState dfa')
    let newFinalS = updateFinalStates groups (finalStates dfa') []
    let newTrans = updateTransitions groups (sort $ transitions dfa') (length $ alphabet dfa')

    let dfa'' = updateDFA dfa' newStates initS newFinalS newTrans
    -- TODO remove findAndRemoveSinkState
    --findAndRemoveSinkState dfa''
    dfa''


-- groups represent equivalence classes
getGroups :: DFA -> [[State]]
getGroups dfa' = do
    -- two initialization groups: final states and non final states
    let nonFinalStates = [[x | x <- states dfa', x `notElem` finalStates dfa']]
    -- remove empty lists
    let initGroups = filter (not . null) $ finalStates dfa' : nonFinalStates

    findMinimalGroups dfa' [[]] initGroups

findMinimalGroups :: DFA -> [[State]] -> [[State]] -> [[State]]
findMinimalGroups dfa' prevGroups groups = do
    -- until there are no new groups
    if length prevGroups == length groups
        then groups --groups
        else do
            let newGroups' = checkGroups dfa' groups groups
            findMinimalGroups dfa' groups newGroups'

-- walk through all groups and compare their next transition state groups
checkGroups :: DFA -> [[State]] -> [[State]] -> [[State]]
checkGroups _ _ [] = []
checkGroups dfa' allGroups groups@(g:gs) = do
    let group' = checkGroup dfa' allGroups g
    if changed group'
        then do
            -- remove old group
            let groups' = filter (\x -> x /= oldGroup group') groups
            -- add new groups
            let groupsAdded = foldr (:) groups' (newGroups group')
            checkGroups dfa' allGroups groupsAdded
        else oldGroup group' : checkGroups dfa' allGroups gs

-- compare each group element transitions
checkGroup :: DFA -> [[State]] -> [State] -> Groups
checkGroup _ _ [] = error internalError
checkGroup dfa' groups group@(x:xs) = if length group == 1
    then Groups False group [[]]
    else do
        let newGroup = compareTransitions dfa' groups x xs []
        if null newGroup
            then Groups False group [[]]
            else Groups True group [newGroup, group \\ newGroup]

compareTransitions :: DFA -> [[State]] -> State -> [State] -> [State] -> [State]
compareTransitions _ _ _ [] newGroup = newGroup
compareTransitions dfa' groups state1 (state2:otherStates) newGroup = do
    let addToNewGroup = foldl (\acc a -> if getTransitionGroupIndex (transitions dfa') groups state1 a == getTransitionGroupIndex (transitions dfa') groups state2 a then acc else True) False (alphabet dfa')
    if addToNewGroup
        then compareTransitions dfa' groups state1 otherStates (state2 : newGroup)
        else compareTransitions dfa' groups state1 otherStates newGroup

getTransitionGroupIndex :: [Transition] -> [[State]] -> State -> Symbol -> Integer
getTransitionGroupIndex trans groups currentState symbol = do

    let tran' = head $ filter (\tran -> cs tran == currentState && is tran == symbol) trans
    getGroupIndex (ns tran') groups

-- find group of state and it's index
getGroupIndex :: State -> [[State]] -> Integer
getGroupIndex state groups = do
    let filtered = filter (elem state) groups
    if null filtered
        then error internalError
        else findIndex' (-1) (head filtered) groups

-- find index of group
findIndex' :: Integer -> [State] -> [[State]] -> Integer
findIndex' _ _ [] = -1
findIndex' i group (x:xs) = do
    if group == x
        then i+1
        else findIndex' (i+1) group xs

-- create new state by group of states, temporarily named by concatenation of original state numbers (debug)
createNewStateName :: [State] -> State
createNewStateName (a:b:xs) = createNewStateName (concatStates a b : xs)
    where concatStates x y = read (show x ++ show y) :: State
createNewStateName a = head a

-- update (rename) init state after minimization
updateInitState :: [[State]] -> State -> State
updateInitState [] initS = initS
updateInitState (group:groups) initS = if initS `elem` group
    then createNewStateName group
    else updateInitState groups initS

-- update (rename) final states after minimization
updateFinalStates :: [[State]] -> [State] -> [State]  -> [State]
updateFinalStates [] _ newFinalS  = newFinalS
updateFinalStates (group:groups) finalS newFinalS = do
    if finalStateInGroup group finalS
        then do
            let newState = createNewStateName group
            updateFinalStates groups finalS (newState : newFinalS)
        else
            updateFinalStates groups finalS newFinalS

finalStateInGroup :: [State] -> [State] -> Bool
finalStateInGroup group = foldr (\finalState acc -> (finalState `elem` group) || acc) False

-- update (rename) transition states after minimization
updateTransitions :: [[State]] -> [Transition] -> Int -> [Transition]
updateTransitions [] trans _ = trans
updateTransitions (group:groups) trans n = 
    if length group > 1
        then updateTransitions groups (updateTrasition trans [] group n) n
        else updateTransitions groups trans n

updateTrasition :: [Transition] -> [Transition] -> [State] -> Int -> [Transition]
updateTrasition [] newTrans _ _ = newTrans
updateTrasition (tran:trans) newTrans group n = do
    let newName = createNewStateName group
    if cs tran `elem` group
        then if n == 0
            then updateTrasition trans newTrans group n
            else if ns tran `elem` group
                then updateTrasition trans (Transition newName (is tran) newName : newTrans) group (n-1)
                else updateTrasition trans (Transition newName (is tran) (ns tran) : newTrans) group (n-1)
        else if ns tran `elem` group
            then updateTrasition trans (Transition (cs tran) (is tran) newName : newTrans) group n
            else updateTrasition trans (tran: newTrans) group n