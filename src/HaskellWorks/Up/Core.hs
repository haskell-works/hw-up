{-# LANGUAGE RankNTypes #-}

module HaskellWorks.Up.Core where

import Control.Selective (Selective (..))

newtype Task k v = Task
  { run :: forall f. Selective f => (k -> f v) -> f v
  }
