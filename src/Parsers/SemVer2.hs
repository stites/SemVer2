module Parsers.SemVer2 where

import Control.Applicative
import Text.Trifecta

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch  = Integer
type Release  = [NumberOrString]
type Metadata = [NumberOrString]

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  minor <- decimal
  patch  <- decimal
  release'  <- some parseNumberOrString
  metadata <- some parseNumberOrString
  return $ SemVer major minor patch release' metadata

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = do
  skipMany whiteSpace
  either' <- (Left <$> integer) <|> (Right <$> some letter)
  case either' of
    Left i  -> return (NOSI i)
    Right s -> return (NOSS s)


