--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Local
  ( local
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Clock
import Copilot.Language.Node

--------------------------------------------------------------------------------

local
  :: (Typed a, Typed b)
  => CStream p a
  -> (CStream p a -> CStream p b)
  -> CStream p b
local (CStream e) f = CStream $ Local e (unCStream . f . CStream)

--------------------------------------------------------------------------------