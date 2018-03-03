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

data Expr model a where
    EntityType     :: model -> Expr model model
    TruthValueType :: Bool -> Expr model Bool
    CompositeType  :: (a -> b) -> Expr model (a -> b)

eval :: Expr model a -> a
eval (EntityType e) = e
eval (TruthValueType t) = t
eval (CompositeType f) = f
