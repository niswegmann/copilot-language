--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Eq
  ( (==)
  , (/=)
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Clock
import Copilot.Language.Node
import Copilot.Language.Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------

(==) :: (P.Eq a, Typed a) => CStream p a -> CStream p a -> CStream p Bool
CStream x == CStream y = CStream $ Op2 (Core.eq typeOf) x y

(/=) :: (P.Eq a, Typed a) => CStream p a -> CStream p a -> CStream p Bool
CStream x /= CStream y = CStream $ Op2 (Core.ne typeOf) x y

--------------------------------------------------------------------------------
