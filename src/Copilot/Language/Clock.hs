--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE TypeFamilies #-}

module Copilot.Language.Clock
  ( Clock (..)
  , CStream (..)
  , Stream
  , Master
  , resample
  ) where

import Copilot.Language.Node (Node (..))

type Stream a = CStream Master a

newtype CStream p a = CStream { unCStream :: Node a }

class Clock p where
  clock :: p -> CStream Master Bool

data Master

instance Clock Master where
  clock _ = CStream (Const True)

resample :: CStream p1 a -> CStream p2 a
resample = CStream . unCStream

{-
class Clock w where
  type Master w :: *
  clock :: (Master w ~ master, Clock master) => w -> CStream master Bool

instance Clock () where
  type Master () = ()
  clock _        = undefined

data MainClk

instance Clock MainClk where
  type Master MyClk = ()
  clock _           = undefined
-}
