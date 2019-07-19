{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
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
  ConsP :: Option r (a -> b) -> Parser a -> Parser b

data Option r a = Option
  { _optMain    :: OptReader r
  , _optDefault :: Maybe a
  , _optShow    :: Bool
  , _optHelp    :: String
  , _optMetaVar :: String
  , _optCont    :: r -> Maybe (Parser a)
  } deriving Functor

instance Functor Parser where
  fmap f (NilP x)      = NilP (f x)
  fmap f (ConsP opt p) = ConsP (fmap (f.) opt) p

instance Applicative Parser where
  pure = NilP
  NilP f <*> p = fmap f p
  ConsP opt p1 <*> p2 = ConsP (fmap uncurry opt) $ (,) <$> p1 <*> p2

data OptReader a
  = OptReader [OptName] (String -> Maybe a)
  | FlagReader [OptName] !a
  | ArgReader (String -> Maybe a)
  | CmdReader [String] (String -> Maybe (ParserInfo a))
  deriving Functor

data OptName = OptShort !Char
  | OptLong !String
  deriving (Eq, Ord)

