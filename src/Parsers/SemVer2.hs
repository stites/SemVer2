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
  _     <- skipOptional whiteSpace
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
  opt <- optional (char '-' >> parseNumberOrString `sepBy` (symbol "."))
  case opt of
    Just nos' -> return nos'-- nos'
    Nothing   -> return []

parseExplicitInteger :: Parser Integer
parseExplicitInteger = integer <* notFollowedBy letter

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = do
  either' <- try (Left <$> parseExplicitInteger) <|> (Right <$> some alphaNum)
  case either' of
    Left i  -> return (NOSI i)
    Right s -> return (NOSS s)


