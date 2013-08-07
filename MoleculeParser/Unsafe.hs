{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MoleculeParser.Unsafe ( dist, mr, he, group, (<~), (#) ) where

-- base
import Prelude                      ( Double )

import Control.Monad                ( Monad(..), mapM_ )

import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    , modifyIORef )

import Data.Function                ( ($), (.) )
import Data.String                  ( IsString(..), String )
import Data.Char                    ( Char )

import System.IO                    ( IO )
import System.IO.Unsafe             ( unsafePerformIO )

-- mtl
import Control.Monad.State.Class    ( MonadState(..) )

-- internal
import MassDistribution             ( MassDistribution, massResolution, massDistribution )
import MoleculeParser               ( mkGroup, halfEmpiricalParser )
import Rules
    ( HalfEmpirical(..)
    , Simplifyable(..)
    , Rule(..)
    , Isotope(..)
    , RuleBook
    , molecularMassM
    , insertSymbol
    , (<~) )

import Data.Tree                    ( Tree' )

instance MonadState [Tree' Char Rule] IO where
    get = readIORef ruleBook
    put = writeIORef ruleBook

instance IsString HalfEmpirical where
    fromString = unsafePerformIO . halfEmpiricalParser

ruleBook :: IORef RuleBook
ruleBook = unsafePerformIO $ do
    rules <- newIORef []
    mapM_ (\(s, r) -> modifyIORef rules (insertSymbol s r)) $
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
        , ("Et", Group "C2H5")
        , ("Ph", Group "C6H5")
        , ("Me", Group "CH3")
        , ("Ac", Group "CH3CO")
        , ("Bu", Group "C4H9")
        , ("Boc", Group "tBuCO2")
        , ("Bn", Group "PhCH2")
        , ("Bz", Group "PhCO")
        , ("Cbz", Group "PhCH2CO2")
        , ("TMS", Group "Me3Si")
        ]
    return rules

(#) :: a -> (a -> a) -> a
(#) v f = f v

mr :: HalfEmpirical -> Double
mr = unsafePerformIO . molecularMassM

dist :: Simplifyable a => a -> MassDistribution
dist = unsafePerformIO . massDistribution

he :: String -> HalfEmpirical
he = unsafePerformIO . halfEmpiricalParser

res :: Double -> MassDistribution -> MassDistribution
res = massResolution

group :: String -> Rule
group = unsafePerformIO . mkGroup
