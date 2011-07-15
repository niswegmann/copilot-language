--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# OPTIONS -fno-warn-orphans #-}

-- |

module Copilot.Language.Operators.Num () where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Clock
import Copilot.Language.Prelude
import Copilot.Language.Node
import qualified Prelude as P

--------------------------------------------------------------------------------

-- | Dummy instance in order to make 'CStream' an instance of 'Num'.
instance Show (CStream p a) where
  show _ = "Stream"

--------------------------------------------------------------------------------

-- | Dummy instance in order to make 'CStream' an instance of 'Num'.
instance P.Eq (CStream p a) where
  (==) = error "'Prelude.(==)' isn't implemented for CStreams!"
  (/=) = error "'Prelude.(/=)' isn't implemented for CStreams!"

--------------------------------------------------------------------------------

instance (Typed a, Num a) => Num (CStream p a) where
  CStream u1 + CStream u2 = CStream $ Op2 (Core.add typeOf) u1 u2
  CStream u1 - CStream u2 = CStream $ Op2 (Core.sub typeOf) u1 u2
  CStream u1 * CStream u2 = CStream $ Op2 (Core.mul typeOf) u1 u2
  abs = CStream . Op1 (Core.abs typeOf) . unCStream
  signum = CStream . Op1 (Core.sign typeOf) . unCStream
  fromInteger = CStream . Const . fromInteger

--------------------------------------------------------------------------------

