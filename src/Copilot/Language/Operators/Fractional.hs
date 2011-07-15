--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# OPTIONS -fno-warn-orphans #-}

-- |

module Copilot.Language.Operators.Fractional () where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Clock
import Copilot.Language.Node
import Copilot.Language.Operators.Num ()
import Copilot.Language.Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------

instance (Typed a, Fractional a) => Fractional (CStream p a) where
  CStream x / CStream y = CStream $ Op2 (Core.fdiv typeOf) x y
  recip (CStream x)     = CStream $ Op1 (Core.recip typeOf) x
  fromRational          = CStream . Const . fromRational
