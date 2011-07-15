--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Integral
  ( div
  , mod
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Clock
import Copilot.Language.Node
import Copilot.Language.Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------

div :: (Typed a, P.Integral a) => CStream p a -> CStream p a -> CStream p a
(CStream x) `div` (CStream y) = CStream $ Op2 (Core.div typeOf) x y

mod :: (Typed a, P.Integral a) => CStream p a -> CStream p a -> CStream p a
(CStream x) `mod` (CStream y) = CStream $ Op2 (Core.mod typeOf) x y

--------------------------------------------------------------------------------
