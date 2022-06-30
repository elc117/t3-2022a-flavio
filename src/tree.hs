data Tree a = Node a (Tree a) (Tree a) | Empty deriving Show

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node n t1 t2) = Node (f n) (fmap f t1) (fmap f t2)

