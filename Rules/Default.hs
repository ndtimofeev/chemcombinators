{-# LANGUAGE NoImplicitPrelude #-}

module Rules.Default where

-- base
import Control.Monad        ( Monad(..), mapM_ )
import Control.Arrow        ( (>>>) )

import Data.Function        ( (.), ($), flip )

-- mtl
import Control.Monad.State  ( StateT(..), execState )

-- internal
import MoleculeParser       ( halfEmpiricalParser )
import Rules
    ( RuleBook
    , Rule(..)
    , Isotope(..)
    , HalfEmpirical(..)
    , insertSymbolM )

defaultRules :: RuleBook
defaultRules = atomsRules >>> groupRules $ []

atomsRules :: RuleBook -> RuleBook
atomsRules rules = flip execState rules $ mapM_ (\(sym, rule) -> insertSymbolM sym rule)
    [ ("H",  Atom 1  1.008 [Stable 0.0115 2.0141, Stable 99.9885 1.0078])
    , ("D",  Atom 1  2.014 [])
    , ("Li", Atom 3  6.941 [Stable 7.5 6.0151, Stable 92.5 7.016])
    , ("B",  Atom 5  10.811 [Stable 19.9 10.0129, Stable 80.1 11.0093])
    , ("C",  Atom 6  12.011 [Stable 1.07 13.0033, Stable 98.93 12])
    , ("N",  Atom 7  14.007 [Stable 99.635 14.0030, Stable 0.365 15.0001])
    , ("O",  Atom 8  15.999 [Stable 99.759 15.9949, Stable 0.037 16.9991, Stable 0.204 17.9991])
    , ("F",  Atom 9  18.998 [])
    , ("Na", Atom 11 22.989 [])
    , ("Mg", Atom 12 24.305 [])
    , ("Al", Atom 13 26.981 [])
    , ("Si", Atom 14 28.085 [])
    , ("P",  Atom 15 30.973 [])
    , ("S",  Atom 16 32.066 [])
    , ("Cl", Atom 17 35.452 [Stable 75.78 34.9688, Stable 24.22 36.9659])
    , ("K",  Atom 19 39.098 [])
    , ("Ca", Atom 20 40.078 [])
    , ("Cu", Atom 29 65.546 [])
    , ("Br", Atom 35 79.904 [])
    , ("Ag", Atom 47 107.8682 [])
    , ("Sn", Atom 50 118.710 [])
    , ("I",  Atom 53 126.904 [])
    ]

groupRules :: RuleBook -> RuleBook
groupRules rules = flip execState rules $ mapM_ (\(sym, str) -> halfEmpiricalParser str >>= insertSymbolM sym . Group)
    [ ("Et", "C2H5")
    , ("Ph", "C6H5")
    , ("Me", "CH3")
    , ("Ac", "CH3CO")
    , ("Bu", "C4H9")
    , ("Boc", "BuCO2")
    , ("Bn", "PhCH2")
    , ("Bz", "PhCO")
    , ("Cbz", "PhCH2CO2")
    , ("TMS", "Me3Si")
    , ("Gly", "HNCH2CO")
    , ("Ala", "HNCHMeCO")
    , ("bAla", "HNCH2CH2CO")
    , ("Val", "HNCHCHMe2CO")
    , ("Ile", "HNCHCHMeEtCO")
    , ("Leu", "HNCHCH2CHMe2CO")
    , ("Pro", "NCH2CH2CH2CHCO")
    , ("Ser", "HNCHCH2OHCO")
    , ("Thr", "HNCHCHMeOHCO")
    , ("Cys", "HNCHCH2SHCO")
    , ("Met", "HNCHCH2CH2SMeCO")
    , ("Asp", "HNCHCH2COOHCO")
    , ("Asn", "HNCHCH2CONH2CO")
    , ("Glu", "HNCHCH2CH2COOHCO")
    , ("Gln", "HNCHCH2CH2CONH2CO")
    , ("Lys", "HNCHCH2CH2CH2CH2NH2CO")
    , ("Arg", "HNCHCH2CH2CH2NHCNHNH2CO")
    , ("His", "HNCHCH2C3H3N2CO")
    , ("Phe", "HNCHCH2PhCO")
    , ("Tyr", "HNCHCH2C6H4OHCO")
    , ("Trp", "HNCHCH2C8H6NCO")
    ]
