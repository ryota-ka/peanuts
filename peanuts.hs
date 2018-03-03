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

like :: Expr Peanuts (Peanuts -> Peanuts -> Bool)
like = CompositeType $ \object subject -> case (subject, object) of
    ( Snoopy    , Woodstock ) -> True
    ( Snoopy    , Charlie   ) -> True
    ( Charlie   , Snoopy    ) -> True
    ( Charlie   , Sally     ) -> True
    ( Charlie   , Schroeder ) -> True
    ( Woodstock , Snoopy    ) -> True
    ( Sally     , Lucy      ) -> True
    ( Lucy      , Sally     ) -> True
    ( Lucy      , Lucy      ) -> True
    ( Linus     , Snoopy    ) -> True
    ( Linus     , Charlie   ) -> True
    ( Schroeder , Charlie   ) -> True
    _                         -> False

love :: Expr Peanuts (Peanuts -> Peanuts -> Bool)
love = CompositeType $ \object subject -> case (subject, object) of
    ( Lucy  , Schroeder ) -> True
    ( Sally , Linus     ) -> True
    ( Patty , Charlie   ) -> True
    _                     -> False

kiss :: Expr Peanuts (Peanuts -> Peanuts -> Bool)
kiss = CompositeType $ \object subject -> case (subject, object) of
    ( Lucy  , Schroeder ) -> True
    ( Sally , Linus     ) -> True
    _                     -> False

hit :: Expr Peanuts (Peanuts -> Peanuts -> Bool)
hit = CompositeType $ \object subject -> case (subject, object) of
    ( Patty , Charlie ) -> True
    _                   -> False

kick :: Expr Peanuts (Peanuts -> Peanuts -> Bool)
kick = CompositeType $ const (const False)
