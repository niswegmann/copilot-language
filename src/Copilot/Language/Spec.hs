--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

-- |

module Copilot.Language.Spec
  ( Spec
  , runSpec
  , SpecItem
  , Observer (..)
  , observer
  , observers
  , Trigger (..)
  , TriggerArg (..)
  , triggers
  , trigger
  , arg
  ) where

import Control.Monad.Writer
import Data.List (foldl')

import Copilot.Core (Typed)
import qualified Copilot.Core as Core
import Copilot.Language.Clock
import Copilot.Language.Node

--------------------------------------------------------------------------------

type Spec = Writer [SpecItem] ()

--------------------------------------------------------------------------------

runSpec :: Spec -> [SpecItem]
runSpec = execWriter 

--------------------------------------------------------------------------------

observers :: [SpecItem] -> [Observer]
observers = 
  foldl' lets' []
  where
  lets' ls e =
    case e of 
      ObserverItem l -> l : ls
      _              -> ls

triggers :: [SpecItem] -> [Trigger]
triggers = 
  foldl' triggers' []
  where
  triggers' ls e =
    case e of 
      TriggerItem t -> t : ls
      _             -> ls

--------------------------------------------------------------------------------

data SpecItem
  = ObserverItem Observer
  | TriggerItem  Trigger

--------------------------------------------------------------------------------

data Observer where
  Observer
    :: Typed a
    => String
    -> Node a
    -> Observer

--------------------------------------------------------------------------------

observer
  :: Typed a
  => String
  -> CStream Master a
  -> Spec
observer name e = tell [ObserverItem $ Observer name (unCStream e)]

--------------------------------------------------------------------------------

data Trigger where
  Trigger
    :: Core.Name
    -> Node Bool
    -> [TriggerArg]
    -> Trigger

--------------------------------------------------------------------------------

data TriggerArg where
  TriggerArg
    :: Typed a
    => Node a
    -> TriggerArg

--------------------------------------------------------------------------------

trigger
  :: String
  -> CStream Master Bool
  -> [TriggerArg]
  -> Spec 
trigger name e args = tell [TriggerItem $ Trigger name (unCStream e) args]

--------------------------------------------------------------------------------

arg :: Typed a => CStream Master a -> TriggerArg
arg = TriggerArg . unCStream

--------------------------------------------------------------------------------
