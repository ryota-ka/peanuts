{-# LANGUAGE GADTs #-}

module Peanuts where

data Peanuts
    = Snoopy
    | Woodstock
    | Charlie
    | Sally
    | Lucy
    | Linus
    | Patty
    | Schroeder
    deriving (Eq, Show)

data Expr a where
    EntityType     :: Peanuts -> Expr Peanuts
    TruthValueType :: Bool -> Expr Bool
    CompositeType  :: (a -> b) -> Expr (a -> b)

eval :: Expr a -> a
eval (EntityType e) = e
eval (TruthValueType t) = t
eval (CompositeType f) = f
