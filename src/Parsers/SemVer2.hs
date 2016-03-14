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
  _     <- delim
  minor <- decimal
  _     <- delim
  patch <- decimal
  release' <- parseSemVerOptionals '-'
  metadata <- parseSemVerOptionals '+'
  return $ SemVer major minor patch release' metadata

delim :: Parser Char
delim = char '.'

parseSemVerOptionals :: Char -> Parser [NumberOrString]
parseSemVerOptionals sep = option [] (char sep >> parseNumberOrString `sepBy` delim)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = try (NOSI <$> validInteger) <|> (NOSS <$> some validChar)

validInteger :: Parser Integer
validInteger = natural <* notFollowedBy validChar

validChar :: Parser Char
validChar = alphaNum <|> char '-'


