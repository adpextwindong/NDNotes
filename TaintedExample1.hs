-- you could have a single type instead and use a phantom type along with datakinds to signal the 'taintedness' - Pantsu}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

import GHC.Generics
import Data.Proxy
import Data.Typeable
import Data.Type.Equality
import Data.Maybe

data Taintedness
  = Untainted
  | Tainted
  deriving (Eq, Show, Typeable)

type family CombineTaintedness (a :: Taintedness) (b :: Taintedness) :: Taintedness where
    'Tainted `CombineTaintedness` _ = 'Tainted
    _ `CombineTaintedness` 'Tainted = 'Tainted
    _ `CombineTaintedness` _ = 'Untainted

type family IsProofMarked' (t :: Taintedness) a :: Bool where
    IsProofMarked' 'Tainted _ = 'True
    IsProofMarked' 'Untainted _  = 'False

type IsProofMarked t a = (IsProofMarked' t a ~ 'True)

type TEQShow a = (Typeable a, Eq a, Show a)

-- Kind-index GADT
data Expr (tainted :: Taintedness) a where
  Base :: (TEQShow a) => Proxy tainted -> a -> Expr tainted a
  Uncurse :: (TEQShow a, TEQShow b, Typeable t, Typeable u) => Expr t a -> Expr u b -> Expr u b
  deriving Typeable

--Compare the type level
exprCmp :: (Typeable a, Typeable b, Typeable t, Typeable u, Eq a, Eq b) => Expr t a -> Expr u b -> Bool
exprCmp (x :: Expr t a) (y:: Expr u b) = (lt == rt) --TODO use a specialized standalone instance for eq or something
                                     && (case cast y of
                                            Just ny -> exprValueCmp x ny
                                            Nothing -> False)
                                     where lt = typeOf x
                                           rt = typeOf y

--Compare at the value level
exprValueCmp :: forall t a. (Typeable t, Eq a) => Expr t a -> Expr t a -> Bool
exprValueCmp (Base _ x) (Base _ y) = x == y
exprValueCmp (Uncurse x y) (Uncurse a b) = exprValueCmp y b
                                        && case eqT @(Expr t a) @(Expr t a) of
                                                Just Refl -> exprCmp x a
                                                Nothing -> False
exprValueCmp _ _ = False

--Only works for matching types, can't compare untained and tained. Use ExprCmp
instance (Typeable t, Typeable a, Eq a) => Eq (Expr t a) where
  (==) = exprCmp

deriving instance (Show a) => Show (Expr t a)

ut = Proxy @'Untainted
tt = Proxy @'Tainted
