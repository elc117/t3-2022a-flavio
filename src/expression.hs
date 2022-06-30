{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
import Language.Haskell.TH (nameBase)

data Exp =  Exp Double  |
            Pow Exp Exp |
            Log Exp Exp |
            Mul Exp Exp |
            Div Exp Exp |
            Add Exp Exp |
            Sub Exp Exp

instance Show Exp where
    show (Exp n) = show n 
    show (Pow n m) = "(" ++ show n ++ " ^ " ++ show m ++ ")"
    show (Log n m) = "(" ++ "Log(" ++ show n ++ ")(" ++ show m ++"))"
    show (Mul n m) = "(" ++ show n ++ " * " ++ show m ++ ")"
    show (Div n m) = "(" ++ show n ++ " / " ++ show m ++ ")"
    show (Add n m) = "(" ++ show n ++ " + " ++ show m ++ ")"
    show (Sub n m) = "(" ++ show n ++ " - " ++ show m ++ ")"

instance Eq Exp where
    n == m          = value n == value m

class Eval a where
    value :: a -> Double

instance Eval Exp where
    value (Exp n) = n
    value (Pow n m) = value n ** value m
    value (Log n m) = logBase (value n) (value m)
    value (Mul n m) = value n * value m
    value (Div n m) = value n / value m
    value (Add n m) = value n + value m
    value (Sub n m) = value n - value m
