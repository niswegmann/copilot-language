--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Mux
  ( mux
  , ifThenElse
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Clock
import Copilot.Language.Node
import Copilot.Language.Prelude
import Prelude ()

--------------------------------------------------------------------------------

mux :: Typed a => CStream p Bool -> CStream p a -> CStream p a -> CStream p a
mux (CStream b) (CStream t) (CStream f) = CStream $ Op3 (Core.mux typeOf) b t f

--------------------------------------------------------------------------------

ifThenElse :: Typed a => Stream Bool -> Stream a -> Stream a -> Stream a
ifThenElse = mux

--------------------------------------------------------------------------------
