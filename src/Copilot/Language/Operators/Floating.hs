--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# OPTIONS -fno-warn-orphans #-}

-- |

module Copilot.Language.Operators.Floating () where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Clock
import Copilot.Language.Node
import Copilot.Language.Operators.Fractional ()
import Copilot.Language.Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------

-- XXX we may not want to precompute these if they're constants if someone is
-- relying on certain floating-point behavior.
instance (Typed a, Floating a) => Floating (CStream p a) where
  pi           = CStream (Const pi)

  exp          = CStream . Op1 (Core.exp typeOf)   . unCStream
  sqrt         = CStream . Op1 (Core.sqrt typeOf)  . unCStream
  log          = CStream . Op1 (Core.log typeOf)   . unCStream
  sin          = CStream . Op1 (Core.sin typeOf)   . unCStream
  tan          = CStream . Op1 (Core.tan typeOf)   . unCStream
  cos          = CStream . Op1 (Core.cos typeOf)   . unCStream
  asin         = CStream . Op1 (Core.asin typeOf)  . unCStream
  atan         = CStream . Op1 (Core.atan typeOf)  . unCStream
  acos         = CStream . Op1 (Core.acos typeOf)  . unCStream
  sinh         = CStream . Op1 (Core.sinh typeOf)  . unCStream
  tanh         = CStream . Op1 (Core.tanh typeOf)  . unCStream
  cosh         = CStream . Op1 (Core.cosh typeOf)  . unCStream
  asinh        = CStream . Op1 (Core.asinh typeOf) . unCStream
  atanh        = CStream . Op1 (Core.atanh typeOf) . unCStream
  acosh        = CStream . Op1 (Core.acosh typeOf) . unCStream

  CStream x ** CStream y          = CStream $ Op2 (Core.pow typeOf)  x y
  logBase (CStream x) (CStream y) = CStream $ Op2 (Core.logb typeOf) x y
