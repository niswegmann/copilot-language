--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Constant
  ( constant
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Clock
import Copilot.Language.Node

--------------------------------------------------------------------------------

constant :: Typed a => a -> CStream p a
constant = CStream . Const

--------------------------------------------------------------------------------