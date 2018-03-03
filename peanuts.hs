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
    deriving (Bounded, Enum, Eq, Show)

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

    FunctionalApplication_l2r :: Expr model (a -> b) -> Expr model a -> Expr model b
    FunctionalApplication_r2l :: Expr model a -> Expr model (a -> b) -> Expr model b

eval :: Expr model a -> a
eval (EntityType e) = e
eval (TruthValueType t) = t
eval (CompositeType f) = f
eval (FunctionalApplication_l2r l r) = (eval l) (eval r)
eval (FunctionalApplication_r2l l r) = (eval r) (eval l)

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

the :: (Bounded model, Enum model) => Expr model ((model -> Bool) -> model)
the = CompositeType $ \predicate ->
    let xs = filter predicate [minBound .. maxBound] in
        case xs of
            [x] -> x
            _   -> error "the given predicate matches more than one entities"

($.) :: Expr model (a -> b) -> Expr model a -> Expr model b
($.) = FunctionalApplication_l2r

(&.) :: Expr model a -> Expr model (a -> b) -> Expr model b
(&.) = FunctionalApplication_r2l

main :: IO ()
main = do
    let entities :: [Peanuts]
        entities = [minBound .. maxBound]

    putStrLn "Linus is a boy"
    print . eval $
        linusE &. boy -- True

    putStrLn "Sally is a boy"
    print . eval $
        sallyE &. boy -- False

    putStrLn "Schroeder is crazy"
    print . eval $
        schroederE &. crazy -- False

    putStrLn "All boys are players"
    let boys = filter (eval boy) entities
    print $
        all (eval player) boys -- True

    putStrLn "All girls are players"
    let girls = filter (eval girl) entities
    print $
        all (eval player) girls -- False

    putStrLn "Some girls are players"
    print $
        any (eval player) girls -- True

    putStrLn "The counselor is a girl"
    print . eval $
        (the $. counselor) &. girl -- True

    putStrLn "Girls are cute"
    print $
        filter (eval girl) entities == filter (eval cute) entities

    putStrLn "Lucy loves Charlie"
    print . eval $
        lucyE &. (love $. charlieE) -- False
