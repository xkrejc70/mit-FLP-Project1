{-
FLP 2022
Variant : DKA-2-MKA
Author  : Jan Krejci
Login   : xkrejc70
-}

{-# LANGUAGE BangPatterns #-}

module Parser (
    parseInput
) where

import Data.List (sort)
import Text.Read (readMaybe)

import Types (DFA(DFA, states, alphabet, initState, finalStates, transitions), State, FinalStates, Symbol, Transition(Transition, cs, is))
import Error (negativeIntError, missingError, invalidAlphabetError, invalidInitStateError, invalidFinalStatesError, invalidStateError, invalidTransitionError, transitionsNotUniqueError, emptyFileError, invalidInitStateFormatError)
import Util (allUnique)

-- parse input into DFA
parseInput :: [String] -> DFA
parseInput input = do
    if null input
        then error emptyFileError
        else do
            if length input < 3
                then error missingError
                else do
                    let !states' = parseStates $ head input
                    let !alphabet' = parseAlphabet $ head $ tail input
                    let !initState' = parseInitState states' $ head $ tail $ tail input
                    let !finalStates' = parseFinalStates states' $ head $ tail $ tail $ tail input
                    let !transitions' = parseTransitions states' alphabet' (getTrans input)
                    DFA {
                        states = states',
                        alphabet = alphabet',
                        initState = initState',
                        finalStates = finalStates',
                        transitions = transitions'
                    }                    

-- states: split by comma, check format, convert to integers, sort
parseStates :: String -> [Integer]
parseStates str = do
    let states' = splitByComma str
    if isNegative states'
        then error negativeIntError
        else do
            let states'' = sort $ map myRead states' :: [Integer]
            if allUnique states''
                then states''
                else error negativeIntError

splitByComma :: String -> [String]
splitByComma "" = []
splitByComma str = let (x, xs) = break (== ',') str
    in if null xs
        then [x]
        else x : splitByComma (tail xs)

isNegative :: [String] -> Bool
isNegative [] = False
isNegative (x:xs)
    | myRead x < 0 = True
    | otherwise = isNegative xs

myRead :: String -> Integer
myRead states' = case readMaybe states' :: Maybe Integer of
    Just x -> x
    Nothing -> error negativeIntError

-- alphabet check
parseAlphabet :: String -> [Symbol]
parseAlphabet alphabet' =
    if alphabetOK alphabet' && allUnique alphabet'
        then alphabet'
        else error invalidAlphabetError

alphabetOK :: String -> Bool
alphabetOK = foldr (\x -> (&&) (x `elem` ['a'..'z'])) True

-- init state check
parseInitState :: [State] -> String -> State
parseInitState states' initState' =
    if myReadInit initState' `elem` states'
        then myReadInit initState'
        else error invalidInitStateError

myReadInit :: String -> Integer
myReadInit states' = case readMaybe states' :: Maybe Integer of
    Just x -> x
    Nothing -> error invalidInitStateFormatError

-- check if transition state was defined
checktState :: [State] -> String -> State
checktState states' state' =
    if myRead state' `elem` states'
        then myRead state'
        else error ("in transition state " ++ state' ++ " - " ++ invalidStateError)

parseFinalStates :: [State] -> String -> FinalStates
parseFinalStates states' finalStates' = do
    let finalStates'' = parseStates finalStates'
    if finalStatesOK finalStates'' states'
        then finalStates''
        else error invalidFinalStatesError

-- check if final states were defined
finalStatesOK :: FinalStates -> [State] -> Bool
finalStatesOK [] [] = True
finalStatesOK _ [] = False
finalStatesOK [] _ = True
finalStatesOK (f:fs) (s:ss)
    | f == s = finalStatesOK fs ss
    | otherwise = finalStatesOK (f:fs) ss

-- transitions: create list of valid transitions
parseTransitions :: [State] -> [Symbol] -> [String] -> [Transition]
parseTransitions states' alphabet' inputTrans = do
    let trans = map (checkTransition states' alphabet') inputTrans
    if allTransUnique trans
        then trans
        else error transitionsNotUniqueError


-- transitions: check symbol, current and next state of every transition
checkTransition :: [State] -> [Symbol] -> String -> Transition
checkTransition states' alphabet' transition = do
    let transition' = splitByComma transition
    if length transition' /= 3
        then error invalidTransitionError
        else do
            let current = checktState states' (head transition')
            let next = checktState states' (last transition')
            let symbol = transition' !! 1
            if length symbol /= 1
                then error invalidTransitionError
                else do
                    let symbol' = head symbol
                    if checkAlphabet symbol' alphabet'
                        -- add new transition
                        then Transition current symbol' next
                        else error invalidTransitionError

getTrans :: [String] -> [String]
getTrans input = do
    if length (tail $ tail $ tail $ tail input) == 0
        then []
        else tail $ tail $ tail $ tail input

checkAlphabet :: Char -> [Symbol] -> Bool
checkAlphabet symbol alphabet' = symbol `elem` alphabet'

allTransUnique :: [Transition] -> Bool
allTransUnique trans = allUnique [((cs tran), (is tran)) | tran <- trans]