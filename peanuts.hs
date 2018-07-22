#!/usr/bin/env stack
-- stack --install-ghc runghc --package hspec

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Peanuts where

import Test.Hspec

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

type Ix = Int

data Expr model a where
    EntityType     :: model -> Expr model model
    TruthValueType :: Bool -> Expr model Bool
    CompositeType  :: (a -> b) -> Expr model (a -> b)

    FunctionalApplication_l2r :: Expr model (a -> b) -> Expr model a -> Expr model b
    FunctionalApplication_r2l :: Expr model a -> Expr model (a -> b) -> Expr model b

    PredicateModification :: Expr model (a -> Bool) -> Expr model (a -> Bool) -> Expr model (a -> Bool)

    Trace   :: Ix -> Expr model model
    Pronoun :: Ix -> Expr model model

type VarAsgmt model = Ix -> model

noSuchAssignment :: Ix -> void
noSuchAssignment ix = error $ "no such assignment: " ++ show ix

emptyVarAsgmt :: VarAsgmt model
emptyVarAsgmt = noSuchAssignment

peanutsAsgmt :: VarAsgmt Peanuts
peanutsAsgmt 1 = Snoopy
peanutsAsgmt 2 = Woodstock
peanutsAsgmt 3 = Charlie
peanutsAsgmt 4 = Sally
peanutsAsgmt 5 = Lucy
peanutsAsgmt 6 = Linus
peanutsAsgmt 7 = Patty
peanutsAsgmt 8 = Schroeder
peanutsAsgmt i = noSuchAssignment i

eval :: VarAsgmt model -> Expr model a -> a
eval _ (EntityType e) = e
eval _ (TruthValueType t) = t
eval _ (CompositeType f) = f
eval g (FunctionalApplication_l2r l r) = (eval g l) (eval g r)
eval g (FunctionalApplication_r2l l r) = (eval g r) (eval g l)
eval g (PredicateModification p q) = \x -> eval g p x && eval g q x
eval g (Trace i) = g i
eval g (Pronoun i) = g i

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

(.&&.) :: Expr model (a -> Bool) -> Expr model (a -> Bool) -> Expr model (a -> Bool)
(.&&.) = PredicateModification

main :: IO ()
main = hspec $ do
    let entities :: [Peanuts]
        entities = [minBound .. maxBound]

    describe "Linus is a boy" $ do
        it "is true" $ do
            eval emptyVarAsgmt (linusE &. boy) `shouldBe` True

    describe "Sally is a boy" $ do
        it "is true" $ do
            eval emptyVarAsgmt (sallyE &. boy) `shouldBe` False

    describe "Schroeder is crazy" $ do
        it "is false" $ do
            eval emptyVarAsgmt (schroederE &. crazy) `shouldBe` False

    describe "All boys are players" $ do
        let boys = filter (eval emptyVarAsgmt boy) entities
        it "is true" $ do
            all (eval emptyVarAsgmt player) boys `shouldBe` True

    describe "All girls are players" $ do
        let girls = filter (eval emptyVarAsgmt girl) entities
        it "is false" $ do
            all (eval emptyVarAsgmt player) girls `shouldBe` False

    describe "Some girls are players" $ do
        let girls = filter (eval emptyVarAsgmt girl) entities
        it "is true" $ do
            any (eval emptyVarAsgmt player) girls `shouldBe` True

    describe "The counselor is a girl" $ do
        it "is true" $ do
            eval emptyVarAsgmt ((the $. counselor) &. girl) `shouldBe` True

    describe "Girls are cute" $ do
        it "is true" $ do
            filter (eval emptyVarAsgmt girl) entities `shouldBe` filter (eval emptyVarAsgmt cute) entities

    describe "Lucy loves Charlie" $ do
        it "is false" $ do
            eval emptyVarAsgmt (lucyE &. (love $. charlieE)) `shouldBe` False

    describe "Sally is a cute girl" $ do
        it "is true" $ do
            eval emptyVarAsgmt (sallyE &. (cute .&&. girl)) `shouldBe` True

    describe "Snoopy is a crazy dog" $ do
        it "is false" $ do
            eval emptyVarAsgmt (snoopyE &. (crazy .&&. dog)) `shouldBe` False

    describe "She(4) loves him(6)" $ do
        let she = Pronoun 4
            him = Pronoun 6
        it "is true" $ do
            eval peanutsAsgmt (she &. (love $. him)) `shouldBe` True
