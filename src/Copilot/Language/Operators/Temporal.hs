--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Copilot.Language.Operators.Temporal
  ( (++)
  , drop
  , guarded
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Clock
import Copilot.Language.Node
import Copilot.Language.Prelude
import Prelude ()

--------------------------------------------------------------------------------

infixr 3 ++

(++) :: forall a p . (Clock p, Typed a) => [a] -> CStream p a -> CStream p a
xs ++ e = CStream $ Append xs g (unCStream e)
  where
    g = case clock (undefined :: p) of
      CStream (Const True) -> Nothing
      CStream e1           -> Just e1

guarded :: Typed a => [a] -> Stream Bool -> Stream a -> Stream a
guarded xs (CStream g) (CStream e) = CStream $ Append xs (Just g) e

drop :: Typed a => Int -> CStream p a -> CStream p a
drop i = CStream . Drop (fromIntegral i) . unCStream

--------------------------------------------------------------------------------