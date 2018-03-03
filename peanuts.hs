{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

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

snoopyE, woodstockE, charlieE, sallyE, lucyE, linusE, pattyE, schroederE :: Expr Peanuts Peanuts
snoopyE    = EntityType Snoopy
woodstockE = EntityType Woodstock
charlieE   = EntityType Charlie
sallyE     = EntityType Sally
lucyE      = EntityType Lucy
linusE     = EntityType Linus
pattyE     = EntityType Patty
schroederE = EntityType Schroeder

data Expr model a where
    EntityType     :: model -> Expr model model
    TruthValueType :: Bool -> Expr model Bool
    CompositeType  :: (a -> b) -> Expr model (a -> b)

eval :: Expr model a -> a
eval (EntityType e) = e
eval (TruthValueType t) = t
eval (CompositeType f) = f

boy :: Expr Peanuts (Peanuts -> Bool)
boy = CompositeType $ \case
    Charlie   -> True
    Linus     -> True
    Schroeder -> True
    _         -> False

girl :: Expr Peanuts (Peanuts -> Bool)
girl = CompositeType $ \case
    Sally -> True
    Lucy  -> True
    Patty -> True
    _     -> False

dog :: Expr Peanuts (Peanuts -> Bool)
dog = CompositeType $ \case
    Snoopy -> True
    _      -> False

bird :: Expr Peanuts (Peanuts -> Bool)
bird = CompositeType $ \case
    Woodstock -> True
    _         -> False

player :: Expr Peanuts (Peanuts -> Bool)
player = CompositeType $ \case
    Snoopy    -> True
    Charlie   -> True
    Lucy      -> True
    Linus     -> True
    Patty     -> True
    Schroeder -> True
    _         -> False

cute :: Expr Peanuts (Peanuts -> Bool)
cute = CompositeType $ \case
    Sally -> True
    Lucy  -> True
    Patty -> True
    _     -> False

crazy :: Expr Peanuts (Peanuts -> Bool)
crazy = CompositeType $ const False

counselor :: Expr Peanuts (Peanuts -> Bool)
counselor = CompositeType $ \case
    Lucy -> True
    _    -> False
