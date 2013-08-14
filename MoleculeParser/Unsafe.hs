{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module MoleculeParser.Unsafe
    ( Uni
    , Deci
    , Centi
    , Milli
    , Micro
    , Pico
    , Nano
    , HighPrecision
    , Precision
    , FixedDistribution
    , Distribution
    , dist
    , mr
    , he
    , group
    , (<~) )
where

-- base
import Prelude                      ( Fractional(..), Double )

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
import Data.Ord                     ( Ord(..) )
import Data.Eq                      ( Eq(..) )
import Data.Fixed                   ( Uni, Deci, Centi, Milli, Micro, Pico, Nano )

import System.IO                    ( IO )
import System.IO.Unsafe             ( unsafePerformIO )

-- mtl
import Control.Monad.State.Class    ( MonadState(..) )

-- internal
import MassDistribution
    ( MassDistribution
    , HighPrecision
    , FixedDistribution
    , Distribution
    , Precision
    , massDistribution )

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
import Rules.Default                ( defaultRules )

import Data.Tree                    ( Tree' )

instance MonadState [Tree' Char Rule] IO where
    get = readIORef ruleBook
    put = writeIORef ruleBook

instance IsString HalfEmpirical where
    fromString = unsafePerformIO . halfEmpiricalParser

ruleBook :: IORef RuleBook
ruleBook = unsafePerformIO $ newIORef defaultRules

mr :: HalfEmpirical -> Double
mr = unsafePerformIO . molecularMassM

dist :: (Simplifyable a, Fractional w, Ord w, Fractional p, Eq p) => a -> MassDistribution w p
dist = unsafePerformIO . massDistribution

he :: String -> HalfEmpirical
he = unsafePerformIO . halfEmpiricalParser

group :: String -> Rule
group = unsafePerformIO . mkGroup
