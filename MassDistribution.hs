{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module MassDistribution
    ( MassDistribution
    , FixedDistribution
    , Distribution
    , HighPrecision
    , Precision
    , testDistribution
    , massDistribution )
where

-- base
import Prelude
    ( Integer
    , Float
    , Double
    , Num(..)
    , Integral(..)
    , Fractional(..)
    , Floating(..)
    , realToFrac
    , (^)
    , fromIntegral
    , error )

import Control.Monad            ( Monad(..), fmap, liftM2 )

import Data.Either              ( Either(..) )
import Data.Function            ( ($), (.), flip )
import Data.Maybe               ( Maybe(..) )
import Data.Bool                ( Bool(..), otherwise )
import Data.Ord                 ( Ord(..) )
import Data.Eq                  ( Eq(..) )
import Data.Tuple               ( fst, snd )
import Data.List                ( (++), intercalate, filter, null, zipWith, genericLength )
import Data.Foldable            ( foldl', foldl, foldr, product )
import Data.Fixed               ( Fixed, HasResolution(..) )
import Data.Typeable            ( Typeable(..) )
import Data.Ratio               ( Rational )

import Text.Show                ( Show(..) )
import Text.Read                ( Read(..) )

-- ghc
import GHC.TypeLits             ( Nat, Sing, SingI(..), fromSing, withSing )

-- mtl
import Control.Monad.State      ( MonadState(..) )

-- containers
import Data.Map                 ( Map, insert, insertWith, empty, toList )

-- internal
import Data.Tree                ( lookup )
import Rules

data E4 = E4 deriving Typeable

data E (n :: Nat) = E

instance SingI n => HasResolution (E n) where
    resolution x = withSing $ f x
        where
            f :: p (E n) -> Sing n -> Integer
            f _ s = 10 ^ fromSing s

instance HasResolution E4 where
    resolution _ = 10000

type HighPrecision = Fixed E4
type Precision n = Fixed (E n)
type FixedDistribution w p = MassDistribution (Precision w) (Precision p)
type Distribution p = MassDistribution (Precision 4) (Precision p)

newtype MassDistribution a b = MassDistribution { unDist :: (Map a b) }
    deriving Read

testDistribution :: Num w => MassDistribution m w -> w
testDistribution = foldr (+) 0 . unDist

instance (Show w, Show p) => Show (MassDistribution w p) where
    show (MassDistribution dict) = intercalate "\n" $ do
        (m, p) <- toList dict
        return $ show m ++ "\t" ++ show p

normalizeDistribution :: (Fractional w, Ord w, Fractional p) => Map w p -> Map w p
normalizeDistribution dict =
    foldl (\acc (m, p) -> insert m (p / norm) acc) empty (toList dict)
    where
        norm = foldr (+) 0 dict

massDistribution ::
    ( Simplifyable a
    , MonadState RuleBook m
    , Fractional w, Ord w, Fractional p, Eq p) => a -> m (MassDistribution w p)
massDistribution val = do
    (Empirical dict) <- simplify val
    rules            <- get
    return $ MassDistribution $ normalizeDistribution $
        foldl' (\acc (m, p) -> insertWith (+) (realToFrac m) p acc) empty $
            foldl' (\acc v -> filter (\(_, p) -> p /= 0) $ liftM2 iter (go rules v) acc) [(0, 1)] (toList dict)
    where
        getValue :: Fractional p => RuleBook -> Symbol -> Either Rational [(p, Rational)]
        getValue rules sym = case lookup rules sym of
            Nothing                         -> error "Unexpected!!!"
            Just (Group _)                  -> error "Other shit happens"
            Just (Atom _ w is)
                | null (filter isStable is) -> Left $ realToFrac w
                | otherwise                 -> Right $ do
                    i <- is
                    case i of
                        Stable p m -> return (realToFrac (p / 100), realToFrac m)
                        Unstable _ -> []


        iter :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
        iter a b = (fst a + fst b, snd a * snd b)

        kIndex :: Integer -> Integer -> [[Integer]]
        kIndex x 1 = [[x]]
        kIndex x y = do
            v  <- [0..x]
            vs <- kIndex (x - v) (y - 1)
            return (v:vs)

        go :: (Fractional p, Eq p) => RuleBook -> (Symbol, Integer) -> [(Rational, p)]
        go rules (sym, i) = case getValue rules sym of
            Left w   -> [(w * fromIntegral i, 1)]
            Right is -> flip fmap (kIndex i (genericLength is)) $ \xs ->
                foldl' iter (0, fromIntegral $ kSub i xs) $
                    zipWith (\k (p, m) -> (m * fromIntegral k, p ^ k)) xs is

        kSub :: Integer -> [Integer] -> Integer
        kSub n = foldl' (\acc rep -> acc `div` product [1..rep]) (product [1..n])

        isStable :: Isotope -> Bool
        isStable (Stable _ _) = True
        isStable _            = False
