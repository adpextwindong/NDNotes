-- you could have a single type instead and use a phantom type along with datakinds to signal the 'taintedness' - Pantsu}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

import GHC.Generics
import Data.Proxy
import Data.Typeable

data Taintedness
  = Untainted
  | Tainted
  deriving Eq

type family CombineTaintedness (a :: Taintedness) (b :: Taintedness) :: Taintedness where
    'Tainted `CombineTaintedness` _ = 'Tainted
    _ `CombineTaintedness` 'Tainted = 'Tainted
    _ `CombineTaintedness` _ = 'Untainted


type family IsProofMarked' (t :: (Taintedness)) a :: Bool where
    IsProofMarked' 'Tainted _ = 'True
    IsProofMarked' 'Untainted _  = 'True
    IsProofMarked' _ _ = 'False

type IsProofMarked t a = (IsProofMarked' t a ~ 'True)

-- Kind-index GADT
data Expr (tainted :: Taintedness) a where
  Base :: Proxy tainted -> a -> Expr tainted a
  Uncurse :: (IsProofMarked t a, IsProofMarked u b) => Expr t a -> Expr u b -> Expr u b


equ :: (IsProofMarked t a, IsProofMarked u b, a ~ b) => Expr t a -> Expr u b -> Bool
equ = undefined

--deriving instance (Eq a, Eq b, IsProofMarked t a, IsProofMarked u b, a ~ b) => Eq (Expr t a)
--

data Testr a where
    BTest :: a -> Testr a
    deriving Eq

--equ :: (Eq a, Eq b) => forall a b. Expr 'Tainted a -> Expr 'Tainted b -> Bool
--equ = False

--TODO some sort of equals instance
--https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Type-Equality.html

ut = (Proxy @'Untainted)
tt = (Proxy @'Tainted)
-- :t Base (Proxy @'Untainted) 1 `And` Base (Proxy @'Untainted) 2
--    Base (Proxy @'Untainted) 1 `And` Base (Proxy @'Untainted) 2  :: (Num a, Num b) => Expr 'Untainted (a, b)

-- :t Base (Proxy @'Tainted) 1 `And` Base (Proxy @'Untainted) 2
--    Base (Proxy @'Tainted) 1 `And` Base (Proxy @'Untainted) 2  :: (Num a, Num b) => Expr 'Tainted (a, b)