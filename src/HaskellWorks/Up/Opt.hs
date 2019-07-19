{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADT          #-}
{-# LANGUAGE RankNTypes    #-}

module HaskellWorks.Up.Opt where

data ParserInfo a = ParserInfo
  { infoParser      :: Parser a
  , infoFullDesc    :: Bool
  , infoHeader      :: String
  , infoProgDesc    :: String
  , infoFooter      :: String
  , infoFailureCode :: Int
  } deriving Functor

data Parser a where
  NilP :: a -> Parser a
  ConsP :: Option r (a -> b)
        -> Parser a
        -> Parser b

data Option r a = Option
  { _optMain    :: OptReader r
  , _optDefault :: Maybe a
  , _optShow    :: Bool
  , _optHelp    :: String
  , _optMetaVar :: String
  , _optCont    :: r -> Maybe (Parser a) }
  deriving Functor

data OptReader a
  = OptReader [OptName] (String -> Maybe a)
  | FlagReader [OptName] !a
  | ArgReader (String -> Maybe a)
  | CmdReader [String] (String -> Maybe (ParserInfo a))
  deriving Functor

data OptName = OptShort !Char
  | OptLong !String
  deriving (Eq, Ord)

