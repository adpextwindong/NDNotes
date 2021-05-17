{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies, TypeOperators, TypeApplications #-}

module Main where

type HasShowEq a = (Show a, Eq a)

data TProofExpr a where
    TAxiom        :: (HasShowEq a)              => a -> TProofExpr a
    TAnd          :: (HasShowEq a, HasShowEq b) => ProofExpr a -> ProofExpr b -> TProofExpr (a,b)
    TExtractLeft  :: (HasShowEq a, HasShowEq b) => ProofExpr (a,b) -> TProofExpr a
    TExtractRight :: (HasShowEq a, HasShowEq b) => ProofExpr (a,b) -> TProofExpr b

data ProofExpr a where
    Axiom        :: (HasShowEq a)              => a -> ProofExpr a
    And          :: (HasShowEq a, HasShowEq b) => ProofExpr a -> ProofExpr b -> ProofExpr (a,b)
    ExtractLeft  :: (HasShowEq a, HasShowEq b) => ProofExpr (a,b) -> ProofExpr a
    ExtractRight :: (HasShowEq a, HasShowEq b) => ProofExpr (a,b) -> ProofExpr b
    DischargeImply :: (HasShowEq a, HasShowEq b) => TProofExpr a -> b -> ProofExpr b

deriving instance (Show a) => Show (ProofExpr a)
deriving instance (Show a) => Show (TProofExpr a)

main :: IO ()
main = putStrLn "Hello, Haskell!"
