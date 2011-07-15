--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Boolean
  ( (&&)
  , (||)
  , not
  , true
  , false
  , xor
  , (==>)
  ) where

import qualified Copilot.Core as Core
import Copilot.Language.Clock
import Copilot.Language.Node
import Copilot.Language.Prelude
import Copilot.Language.Operators.Constant (constant)
import Prelude ()

--------------------------------------------------------------------------------

true :: Stream Bool
true = constant True

false :: Stream Bool
false = constant False

infix 5 &&

(&&) :: CStream p Bool -> CStream p Bool -> CStream p Bool
CStream x && CStream y = CStream $ Op2 Core.and x y

infix 5 ||

(||) :: CStream p Bool -> CStream p Bool -> CStream p Bool
CStream x || CStream y = CStream $ Op2 Core.or x y

not :: CStream p Bool -> CStream p Bool
not = CStream . Op1 Core.not . unCStream

xor :: CStream p Bool -> CStream p Bool -> CStream p Bool
x `xor` y = ( not x && y ) || ( x && not y )

(==>) :: CStream p Bool -> CStream p Bool -> CStream p Bool
x ==> y = not x || y
