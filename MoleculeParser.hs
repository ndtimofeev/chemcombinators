-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FunctionalDependencies #-}

module MoleculeParser ( halfEmpiricalParser, mkGroup, simplify ) where

-- base
import Prelude                  ( Integer, Float, Double, Num(..), Fractional(..), (^), fromIntegral, error )

import Control.Monad            ( Monad(..), fmap, liftM )
import Control.Applicative      ( (<|>) )

import Data.Either              ( Either(..) )
import Data.String              ( String )
import Data.Function            ( id, ($), (.) )
import Data.Maybe               ( Maybe(..), maybe, listToMaybe )
import Data.Eq                  ( Eq(..) )
import Data.Bool                ( Bool(..) )
import Data.Ord                 ( Ord(..) )
import Data.Tuple               ( fst )
import Data.Char                ( Char )
import Data.List                ( (++) )

import Text.Show                ( Show(..) )
import Text.Read                ( read )

-- mtl
import Control.Monad.State      ( MonadState(..) )

-- parsec
import Text.Parsec.String       ()
import Text.Parsec.Char         ( digit, letter, char )
import Text.Parsec.Prim         ( Stream(..), ParsecT, runParserT, unexpected, try )
import Text.Parsec.Combinator   ( option, many1, between, eof )

-- internal
import Data.Tree                ( Tree'(..) )
import Rules

data Result a b    = Match a | SubMatch a b | Prefix b | NotMatch deriving Show

checkPrefix :: Eq k => k -> PrefixTree' k v -> Result v (PrefixTree' k v)
checkPrefix key tree = maybe NotMatch id $ listToMaybe $ do
    (Node' k mval subforest) <- tree
    case (k == key, subforest, mval) of
        (False, _, _)      -> []
        (_, [], Nothing)   -> []
        (_, xs, Nothing)   -> return $ Prefix xs
        (True, [], Just v) -> return $ Match v
        (True, xs, Just v) -> return $ SubMatch v xs

index :: (Stream t m Char, Monad m) => ParsecT t u m Integer
index = do
    q <- liftM read (many1 digit)
    if q > 1
        then return q
        else unexpected "expected index grater then one"

halfEmpirical :: MonadState RuleBook m => ParsecT String u m HalfEmpirical
halfEmpirical = liftM L $ many1 $ withIndex (liftM S symbol <|> groupedSymbols)

groupedSymbols :: MonadState RuleBook m => ParsecT [Char] u m HalfEmpirical
groupedSymbols = between (char '(') (char ')') halfEmpirical

withIndex :: Monad m => ParsecT [Char] u m HalfEmpirical -> ParsecT [Char] u m HalfEmpirical
withIndex par = do
    val <- par
    option val $ try $ index >>= return . G val

symbol :: MonadState RuleBook m => ParsecT [Char] u m Symbol
symbol = fmap fst symbol'

symbol' :: MonadState RuleBook m => ParsecT [Char] u m (Symbol, Rule)
symbol' = get >>= go "" where
    go :: Monad m => String -> RuleBook -> ParsecT [Char] u m (Symbol, Rule)
    go sym rules = letter >>= \l -> case checkPrefix l rules of
        NotMatch          -> unexpected (show l)
        Match v           -> return (sym ++ [l], v)
        Prefix rules'     -> go (sym ++ [l]) rules'
        SubMatch v rules' -> option (sym ++ [l], v) (try $ go (sym ++ [l]) rules')

halfEmpiricalParser :: MonadState RuleBook m => String -> m HalfEmpirical
halfEmpiricalParser str = do
    eval <- runParserT (halfEmpirical >>= \he -> eof >> return he) () "" str
    case eval of
        Right v -> return v
        Left e  -> error $ show e

mkGroup :: MonadState RuleBook m => String -> m Rule
mkGroup str = liftM Group (halfEmpiricalParser str)
