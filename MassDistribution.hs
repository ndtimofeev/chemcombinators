{-# LANGUAGE FlexibleContexts #-}

module MassDistribution
    ( MassDistribution
    , dynamicRange
    , findMax
    , massResolution
    , testDistribution
    , massDistribution
    , size )
where

import Prelude
    ( Integer
    , Float
    , Double
    , Num(..)
    , Integral(..)
    , Fractional(..)
    , Floating(..)
    , (^)
    , fromIntegral
    , error )

import Control.Monad            ( Monad(..), fmap, liftM2 )

import Data.Either              ( Either(..) )
import Data.Function            ( ($), (.), flip )
import Data.Maybe               ( Maybe(..) )
import Data.Bool                ( Bool(..), (&&), otherwise, not )
import Data.Ord                 ( Ord(..), max )
import Data.Tuple               ( fst, snd )
import Data.List                ( filter, null, zipWith, genericLength )
import Data.Foldable            ( foldl', foldl, foldr, product )

import Text.Show                ( Show(..) )
import Text.Printf
import Text.Read                ( Read(..) )

-- mtl
import Control.Monad.State      ( MonadState(..) )

-- containers
import Data.Map                 ( Map, insert, insertWith, empty, toList )
import qualified Data.Map       ( size, filter, null, findMax )

-- semigroups
import Data.List.NonEmpty       ( fromList )

-- internal
import Data.Tree                ( lookup )
import Rules

newtype MassDistribution = MassDistribution { unDist :: (Map Double Double) }
    deriving Read

size = Data.Map.size . unDist
findMax = Data.Map.findMax . unDist

testDistribution :: MassDistribution -> Double
testDistribution = foldr (+) 0 . unDist

massResolution :: Double -> MassDistribution -> MassDistribution
massResolution d (MassDistribution dict) =
    MassDistribution $ foldl (\acc (m, p) ->
        if not (Data.Map.null acc) && fst (Data.Map.findMax acc) + d > m
            then insertWith (+) (fst (Data.Map.findMax acc)) p acc else insert m p acc)
                empty (toList dict)

dynamicRange :: Double -> MassDistribution -> MassDistribution
dynamicRange d (MassDistribution dict) = MassDistribution $
    Data.Map.filter (\a -> logBase 10 (maxP / a) < d) dict
    where
        maxP :: Double
        maxP = foldl max (snd (Data.Map.findMax dict)) dict

instance Show MassDistribution where
    show (MassDistribution dict) = do
        (m, p) <- toList dict
        printf "%.4f\t%.4f\n" m p

massDistribution :: (Simplifyable a, MonadState RuleBook m) => a -> m MassDistribution
massDistribution val = do
    (Empirical dict) <- simplify val
    rules            <- get
    return $ MassDistribution $
        foldl (\acc (m, p) -> insertWith (+) m (p * 100) acc) empty $
            foldl (\acc v -> liftM2 iter (go rules v) acc) [(0, 1)] (toList dict)
    where
        getValue :: RuleBook -> Symbol -> Either Double [(Double, Double)]
        getValue rules sym = case lookup rules sym of
            Nothing                         -> error "Unexpected!!!"
            Just (Group _)                  -> error "Other shit happens"
            Just (Atom _ w is)
                | null (filter isStable is) -> Left w
                | otherwise                 -> Right $ do
                    i <- is
                    case i of
                        Stable p m -> return (p, m)
                        Unstable _ -> []


        iter :: (Double, Double) -> (Double, Double) -> (Double, Double)
        iter a b = (fst a + fst b, snd a * snd b)

        kIndex :: Integer -> Integer -> [[Integer]]
        kIndex x 1 = [[x]]
        kIndex x y = do
            v  <- [0..x]
            vs <- kIndex (x - v) (y - 1)
            return (v:vs)

        go :: RuleBook -> (Symbol, Integer) -> [(Double, Double)]
        go rules (sym, i) = case getValue rules sym of
            Left w   -> [(w * fromIntegral i, 1)]
            Right is -> flip fmap (kIndex i (genericLength is)) $ \xs ->
                foldl iter (0, fromIntegral $ kSub i xs) $
                    zipWith (\k (p, m) -> (m * fromIntegral k, (p / 100) ^ k)) xs is

        kSub :: Integer -> [Integer] -> Integer
        kSub n = foldl' (\acc rep -> acc `div` product [1..rep]) (product [1..n])

        isStable :: Isotope -> Bool
        isStable (Stable _ _) = True
        isStable _            = False
