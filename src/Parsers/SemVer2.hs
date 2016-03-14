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
  _     <- char '.'
  minor <- decimal
  _     <- char '.'
  patch <- decimal
  release' <- parseSemVerOptionals
  metadata <- parseSemVerOptionals
  return $ SemVer major minor patch release' metadata

parseSemVerOptionals :: Parser [NumberOrString]
parseSemVerOptionals = do
  opt <- optional (char '-' >> some parseNumberOrString)
  case opt of
    Just nos' -> return nos'
    Nothing   -> return []

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = do
  skipMany whiteSpace
  either' <- (Left <$> integer) <|> (Right <$> some letter)
  case either' of
    Left i  -> return (NOSI i)
    Right s -> return (NOSS s)


