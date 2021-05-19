{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import GHC.Generics
import Data.Proxy
import Data.Typeable
import Data.Type.Equality
import Data.Maybe

data PTaintedness -- Burden of Proof
    = Burdened
    | Unburdened
    deriving (Eq, Show, Typeable)

type family CombinePTaintedness (a :: PTaintedness) (b :: PTaintedness) :: PTaintedness where
    'Burdened `CombinePTaintedness` _ = 'Burdened
    _ `CombinePTaintedness` 'Burdened = 'Burdened
    _ `CombinePTaintedness` _ = 'Unburdened

type family IsProofMarked' (t :: PTaintedness) a :: Bool where
    IsProofMarked' 'Burdened _ = 'True
    IsProofMarked' 'Unburdened _ = 'False

type IsProofMarked t a = (IsProofMarked' t a ~ 'True)
type TEQShow a = (Typeable a, Eq a, Show a)

data ProofExpr a where
    Axiom        :: (TEQShow a)              => a -> ProofExpr a

    AndIntro          :: (TEQShow a, TEQShow b) => ProofExpr a -> ProofExpr b -> ProofExpr (a,b)
    ExtractLeftExtro  :: (TEQShow a, TEQShow b) => ProofExpr (a,b) -> ProofExpr a
    ExtractRightExtro :: (TEQShow a, TEQShow b) => ProofExpr (a,b) -> ProofExpr b
    --ImplyIntro     :: Given an Expr B that recursively contains Tainted A annotated Z,
    --               :: (ContainsBurdenZ b a) => b -> (a->b)
    --               Somehow extract the a from b?
    DischargeImplyExtro :: (TEQShow a, TEQShow b) => ProofExpr (a -> b) -> a -> ProofExpr b


main :: IO ()
main = putStrLn "Hello, Haskell!"
