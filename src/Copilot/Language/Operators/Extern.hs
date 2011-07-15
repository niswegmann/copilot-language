--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Extern
  ( extern
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Clock
import Copilot.Language.Node

--------------------------------------------------------------------------------

extern :: Typed a => String -> CStream Master a
extern = CStream . Extern

--------------------------------------------------------------------------------
