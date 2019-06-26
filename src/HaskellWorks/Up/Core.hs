{-# LANGUAGE RankNTypes #-}

module HaskellWorks.Up.Core where

import Control.Selective

newtype Task k v = Task
  { run :: forall f. Selective f => (k -> f v) -> f v
  }

dependenciesOver :: Task k v -> [k]
dependenciesOver task = getOver $ run task (\k -> Over [k])

dependenciesUnder :: Task k v -> [k]
dependenciesUnder task = getUnder $ run task (\k -> Under [k])

type Script k v = k -> Maybe (Task k v)

tar :: Selective f => [f String] -> f String
tar _ = pure ""

compile :: Selective f => [f String] -> f String
compile _ = pure ""

parse :: Selective f => f String -> f Bool
parse _ = pure False

script :: Script FilePath String
script "release.tar" = Just $ Task $ \fetch -> tar [fetch"LICENSE",fetch"exe"]
script "exe" = Just $ Task $ \fetch ->
  let src   = fetch "src.ml"
      cfg   = fetch "config"
      libc  = fetch "lib.c"
      libml = fetch "lib.ml"
  in compile [src, ifS (parse cfg) libc libml]
script _ = Nothing
