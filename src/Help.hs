{-
FLP 2022
Variant : DKA-2-MKA
Author  : Jan Krejci
Login   : xkrejc70
-}

module Help (
    help
) where

-- help message
help :: String
help = "Usage: \n\
\./flp21-fun volby [vstup] \n\
\kde:\n\
\   vstup je jméno vstupního souboru (pokud není specifikováno, program čte\n\
\   standardní vstup) obsahujícího DKA.\n\
\   volba je parametr ovlivňující chování programu:\n\
\       -i na standardní výstup se vypíše načtený a do vaší vnitřní reprezentace\n\
\          převedený DKA.\n\
\       -t na stdout se vypíše MKA."