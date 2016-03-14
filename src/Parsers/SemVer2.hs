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
  release' <- parseSemVerOptionals '-'
  metadata <- parseSemVerOptionals '+'
  return $ SemVer major minor patch release' metadata

parseSemVerOptionals :: Char -> Parser [NumberOrString]
parseSemVerOptionals sep = do
  opt <- optional (char sep >> parseNumberOrString `sepBy` (symbol "."))
  case opt of
    Just nos' -> return nos'-- nos'
    Nothing   -> return []

validInteger :: Parser Integer
validInteger = natural <* notFollowedBy validChar

validChar :: Parser Char
validChar = alphaNum <|> char '-'

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = do
  either' <- try (Left <$> validInteger) <|> (Right <$> some validChar)
  case either' of
    Left i  -> return (NOSI i)
    Right s -> return (NOSS s)


