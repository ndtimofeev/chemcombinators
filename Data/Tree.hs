module Data.Tree where

-- base
import Prelude             ()

import Control.Monad       ( Monad(..), join )
import Control.Applicative ( (<|>) )

import Data.Eq             ( Eq(..) )
import Data.List           ( (++), map, elem, notElem )
import Data.Maybe          ( Maybe(..), listToMaybe )
import Data.Function       ( ($) )
import Data.Bool           ( Bool(..) )

import Text.Show           ( Show(..) )

-- semigroups
import Data.List.NonEmpty  ( NonEmpty(..), uncons )

data Tree a = Node a [Tree a] deriving Show
data Tree' a b = Node' { pice :: a, stored :: (Maybe b), forest :: [Tree' a b] }
    deriving Show

treeValue :: Tree a -> a
treeValue (Node a _) = a

insertValue :: Eq a => [Tree a] -> [a] -> [Tree a]
insertValue tree ls = case ls of
    []      -> tree
    (x:xs)  -> if x `elem` (map treeValue tree)
        then tree ++ [Node x (insertValue [] xs)]
        else do
            v@(Node k f) <- tree
            return $ if k == x
                then Node k (insertValue f xs)
                else v

insertValue' :: Eq a => [Tree' a b] -> NonEmpty a -> b -> [Tree' a b]
insertValue' trie prefix val = if notElem node (map (\(Node' a _ _) -> a) trie)
    then trie ++ [Node' node mval (ntrie [])]
    else do
        t@(Node' k v subtrie) <- trie
        return $ if k == node
            then Node' k (v <|> mval) (ntrie subtrie)
            else t
    where
        (node, subprefix) = uncons prefix
        (mval, ntrie)   = case subprefix of
            Just v  -> (Nothing, \l -> insertValue' l v val)
            Nothing -> (Just val, \l -> l)

lookup :: Eq k => [Tree' k v] -> NonEmpty k -> Maybe v
lookup trie prefix = join $ listToMaybe $ do
    (Node' k mv subtrie) <- trie
    case (k == node, subprefix) of
        (True, Nothing) -> return mv
        (True, Just v)  -> return $ lookup subtrie v
        (False, _)      -> []
    where
        (node, subprefix) = uncons prefix
