--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Ord
  ( (<=)
  , (>=)
  , (<)
  , (>)
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Clock
import Copilot.Language.Node
import Copilot.Language.Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------

(<=) :: (P.Ord a, Typed a) => CStream p a -> CStream p a -> CStream p Bool
CStream x <= CStream y = CStream $ Op2 (Core.le typeOf) x y

(>=) :: (P.Ord a, Typed a) => CStream p a -> CStream p a -> CStream p Bool
CStream x >= CStream y = CStream $ Op2 (Core.ge typeOf) x y

(<)  :: (P.Ord a, Typed a) => CStream p a -> CStream p a -> CStream p Bool
CStream x < CStream y = CStream $ Op2 (Core.lt typeOf) x y

(>)  :: (P.Ord a, Typed a) => CStream p a -> CStream p a -> CStream p Bool
CStream x > CStream y = CStream $ Op2 (Core.gt typeOf) x y

--------------------------------------------------------------------------------
