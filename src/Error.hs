{-
FLP 2022
Variant : DKA-2-MKA
Author  : Jan Krejci
Login   : xkrejc70
-}

module Error (
    invalidArgLength,
    invalidArg,
    fileDoesNotExists,
    negativeIntError,
    emptyFileError,
    missingError,
    invalidAlphabetError,
    invalidInitStateError,
    invalidStateError,
    invalidFinalStatesError,
    invalidTransitionError,
    internalError,
    invalidInitStateFormatError,
    transitionsNotUniqueError
) where
    
-- arguments
invalidArgLength :: String
invalidArgLength = "Invalid argument length!\n"

invalidArg :: String
invalidArg = "Invalid argument "

-- file
fileDoesNotExists :: String
fileDoesNotExists = "No such file: "

-- input format
inputError :: String
inputError = "invalid input format, "

emptyFileError :: String
emptyFileError = "empty file!\n"

missingError :: String
missingError = "missing states, alphabet or init state!\n"

negativeIntError :: String
negativeIntError = inputError ++ "states only unique Non Negative Integers!\n"

invalidAlphabetError :: String
invalidAlphabetError = inputError ++ "alphabet only unique [a-z]!\n"

invalidInitStateError :: String
invalidInitStateError = inputError ++ "init state is not from a set of states!\n"

invalidInitStateFormatError :: String
invalidInitStateFormatError = inputError ++ "init state is not in correct format!\n"

invalidStateError :: String
invalidStateError = inputError ++ "state is not from a set of states!\n"

invalidFinalStatesError :: String
invalidFinalStatesError = inputError ++ "final states error!\n"

invalidTransitionError :: String
invalidTransitionError = inputError ++ "transitions are not in a valid format!\n"

transitionsNotUniqueError :: String
transitionsNotUniqueError = inputError ++ "transitions are not unique\n"

-- others
internalError :: String
internalError = "internal error!\n"
