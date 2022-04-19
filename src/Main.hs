{-
FLP 2022
Variant : DKA-2-MKA
Author  : Jan Krejci
Login   : xkrejc70
-}

module Main where

import Args (checkargsLength, processArgs)
import Parser (parseInput)
import Types (Result(dfa, action), Action(I, T))
import Minimize (minimize)
import Rename (myPrint)

import System.Environment (getArgs)

main :: IO ()
main = do

    -- check program arguments
    args <- getArgs
    checkargsLength args

    -- save action (-t or -i) and parsed input to type Result
    let result = processArgs args

    -- check input format (syntax) and save it as DFA 
    dfa' <- dfa result
    let validDFA = parseInput $ lines dfa'

    -- print given DFA or minimal DFA depending on action
    case action result of
        I -> print validDFA
        T -> myPrint $ minimize validDFA