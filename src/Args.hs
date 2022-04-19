{-
FLP 2022
Variant : DKA-2-MKA
Author  : Jan Krejci
Login   : xkrejc70
-}

module Args (
    checkargsLength,
    processArgs
) where

import Error (invalidArgLength, invalidArg)
import Input (getInput)
import Types (Result(Result), Action(I,T))
import Help (help)

import System.Exit ()

type Arguments = [String]

-- check number of program arguments
checkargsLength :: Arguments -> IO ()
checkargsLength args = do
    case length args of
        x | x < 1 -> error (invalidArgLength ++ help)
        x | x > 2 -> error (invalidArgLength ++ help)
        _ -> return ()

-- process program arguments: get action and input
processArgs :: Arguments -> Result
processArgs [] = error (invalidArgLength ++ help)
processArgs (opt:args) = do
    case opt of
        "-i" -> Result I (getInput args)
        "-t" -> Result T (getInput args)
        _ -> error (invalidArg ++ opt ++ "\n" ++ help)
