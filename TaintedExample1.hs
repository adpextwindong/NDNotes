-- you could have a single type instead and use a phantom type along with datakinds to signal the 'taintedness' - Pantsu}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

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

-- Kind-index GADT
data Expr (tainted :: Taintedness) a where
  Base :: Proxy tainted -> a -> Expr tainted a
  Uncurse :: Expr 'Tainted a -> Expr 'Untainted b -> Expr 'Untainted b

--TODO some sort of equals instance
--https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Type-Equality.html

ut = (Proxy @'Untainted)
tt = (Proxy @'Tainted)
-- :t Base (Proxy @'Untainted) 1 `And` Base (Proxy @'Untainted) 2
--    Base (Proxy @'Untainted) 1 `And` Base (Proxy @'Untainted) 2  :: (Num a, Num b) => Expr 'Untainted (a, b)

-- :t Base (Proxy @'Tainted) 1 `And` Base (Proxy @'Untainted) 2
--    Base (Proxy @'Tainted) 1 `And` Base (Proxy @'Untainted) 2  :: (Num a, Num b) => Expr 'Tainted (a, b)