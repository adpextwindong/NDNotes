-- you can even do funky stuff like turning off constructors
-- which will prevent Stuff ever existing as a tainted Expr because you can only ever construct it if tainted is Untainted

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

import Data.Proxy
import Data.Void
import Data.Kind

data Taintedness
  = Untainted
  | Tainted

type family CombineTaintedness (a :: Taintedness) (b :: Taintedness) :: Taintedness where
    'Tainted `CombineTaintedness` _ = 'Tainted
    _ `CombineTaintedness` 'Tainted = 'Tainted
    _ `CombineTaintedness` _ = 'Untainted

type family OnlyUntainted (a :: Taintedness) :: Type where
  OnlyUntainted 'Tainted = Void
  OnlyUntainted 'Untainted = ()

data Expr (tainted :: Taintedness) a where
  Base :: Proxy tainted -> a -> Expr tainted a
  Uncurse :: Expr 'Tainted a -> Expr 'Untainted b -> Expr 'Untainted b
  And :: Expr ta a -> Expr tb b -> Expr (ta `CombineTaintedness` tb) (a, b)

  Stuff :: Expr tainted a -> OnlyUntainted tainted -> Expr 'Untainted a
  
-- :t Stuff (Base (Proxy @'Tainted) 1)
--    Stuff (Base (Proxy @'Tainted) 1)  :: Num a => Void -> Expr 'Untainted a

-- :t Stuff (Base (Proxy @'Untainted) 1) ()
--    Stuff (Base (Proxy @'Untainted) 1) () :: Num a => Expr 'Untainted a