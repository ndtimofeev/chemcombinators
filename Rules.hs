{-# LANGUAGE FlexibleContexts #-}

module Rules where

-- base
import Prelude                      ( Num(..), Integer, Double, error, otherwise )

import Control.Monad                ( Monad(..), fmap, foldM )

import Data.String                  ( String )
import Data.List                    ( (++), map, concatMap )
import Data.Char                    ( Char )
import Data.Maybe                   ( Maybe(..), catMaybes )
import Data.Foldable                ( foldl, foldr )
import Data.Function                ( ($), (.) )
import Data.Ord                     ( Ord(..) )

import Text.Show                    ( Show(..) )

-- mtl
import Control.Monad.State.Class    ( MonadState(..), modify )

-- containers
import Data.Map                     ( Map, insertWith, unionWith, empty, delete, toList )
import qualified Data.Map           ( lookup )

-- semigroups
import Data.List.NonEmpty           ( NonEmpty(..), fromList )

-- internal
import Data.Tree                    ( Tree'(..), insertValue', lookup )

type Symbol        = String -- NonEmpty Char

data Rule
    = Atom { number :: Integer, weight :: Double, isotopes :: [Isotope] }
    | Group HalfEmpirical
    deriving Show

data Isotope
    = Stable { abundance :: Double, mass :: Double }
    | Unstable { mass :: Double }
    deriving Show

data HalfEmpirical = S Symbol | G HalfEmpirical Integer | L [HalfEmpirical]
newtype  Empirical = Empirical (Map Symbol Integer)

class Simplifyable a where
    simplify :: MonadState RuleBook m => a -> m Empirical

instance Simplifyable HalfEmpirical where
    simplify he = rewrite empty he >>= return . Empirical

rewrite :: MonadState RuleBook m => Map String Integer -> HalfEmpirical -> m (Map String Integer)
rewrite acc (S str)  = do
    rules <- get
    case lookup rules (fromList str) of
        Nothing             -> error "Unexpected!!!"
        Just (Group he)     -> rewrite acc he
        Just (Atom _ _ _)  -> return $ insertWith (+) str 1 acc
rewrite acc (L xs)   = foldM rewrite acc xs
rewrite acc (G he i) = do
    val <- rewrite empty he
    return $ unionWith (+) acc (fmap (*i) val)

instance Show HalfEmpirical where
    show a = case a of
        S s       -> s
        L xs      -> concatMap show' xs
        G (S s) i -> s ++ showIndex i
        G n     i -> "(" ++ show n ++ ")" ++ showIndex i
        where
            show' (L xs) = "(" ++ concatMap show' xs ++ ")"
            show' he     = show he

instance Show Empirical where
    show (Empirical dict) = concatMap (\(s, i) -> s ++ showChemIndex i) $
        catMaybes (fmap (\x -> fmap (\y -> (x,y)) $ Data.Map.lookup x dict) elements) ++ toList (foldr delete dict elements)
        where
            elements = ["C", "H", "O"]

showChemIndex :: Integer -> String
showChemIndex i
    | i < 2     = ""
    | otherwise = showIndex i

showIndex :: Integer -> String
showIndex i = map showDigitIndex (show i)

showDigitIndex :: Char -> Char
showDigitIndex d = case d of
    '0' -> '₀'
    '1' -> '₁'
    '2' -> '₂'
    '3' -> '₃'
    '4' -> '₄'
    '5' -> '₅'
    '6' -> '₆'
    '7' -> '₇'
    '8' -> '₈'
    '9' -> '₉'
    _   -> error "not digit"

molecularMass :: HalfEmpirical -> RuleBook -> Double
molecularMass (G he i) rules = fromInteger i * molecularMass he rules
molecularMass (L ls)   rules = foldl (\a b -> a + molecularMass b rules) 0 ls
molecularMass (S s)    rules = case lookup rules (fromList s) of
    Just (Atom _ m _) -> m
    Just (Group he)   -> molecularMass he rules
    Nothing           -> error "Some thing broken!"

molecularMassM :: MonadState RuleBook m => HalfEmpirical -> m Double
molecularMassM he = get >>= return . molecularMass he

type PrefixTree' a b = [Tree' a b]
type RuleBook        = PrefixTree' Char Rule

(<~) :: MonadState RuleBook m => String -> m Rule -> m Rule
(<~) sym mrule = do
    rule <- mrule
    insertSymbolM sym rule
    return rule

insertSymbol :: String -> Rule -> RuleBook -> RuleBook
insertSymbol sym rule rules = insertValue' rules (fromList sym) rule

insertSymbolM :: MonadState RuleBook m => String -> Rule -> m ()
insertSymbolM sym rule = modify (\xs -> insertValue' xs (fromList sym) rule)
