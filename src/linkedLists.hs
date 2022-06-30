{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

import Data.Semigroup

data List a = Node a (List a) | Empty

-- data List a = Empty
--             | Node {
--                 valor :: a,
--                 filho :: List a
--             } 

class MyListClass a where
    size' :: a ->  Int

instance MyListClass (List a) where
    size' (Node n ns) = 1 + size' ns
    size' Empty = 0



instance Show a => Show (List a) where
    show Empty = "[]"
    show (Node a as) = show a ++ " . " ++ show as


instance (Eq a) => Eq (List a) where
    Empty       == Empty        = True
    Empty       == (Node n _)   = False
    (Node n _)  == Empty        = False
    (Node n ns) == (Node m ms)   = (n == m) && (ns == ns)

instance (Num a) => Num (List a) where
    Empty + Empty = Empty
    (Node n ns) + (Node m ms) = Node (n + m) (ns + ms) 
    n + Empty = n 
    Empty + n = n

instance Functor List where
    fmap f (Node n ns) = Node (f n) (fmap f ns)
    fmap f Empty = Empty 