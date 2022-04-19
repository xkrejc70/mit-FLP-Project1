{-
FLP 2022
Variant : DKA-2-MKA
Author  : Jan Krejci
Login   : xkrejc70
-}

module Input (
    getInput
) where

import Error (fileDoesNotExists)
import Help (help)

import System.Directory (doesFileExist)

type Argument = [String]

-- get input from a file or standard input
getInput :: Argument -> IO String
getInput args = do
    if null args
        then readStdin

        else do
            let file = head args
            readFromFile file

-- read standard input
readStdin :: IO String
readStdin = getContents

-- read from given file (if exists)
readFromFile :: FilePath -> IO String
readFromFile file = do
    fileExists <- doesFileExist file
    if fileExists
        then do
            readFile file
        else
            error (fileDoesNotExists ++ file ++ "\n" ++ help)
