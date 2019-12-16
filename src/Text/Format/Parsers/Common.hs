module Text.Format.Parsers.Common where

import Prelude hiding (null)

import Text.Format.Formatter
import Text.Format.Specifier

import Text.Megaparsec (Parsec, ParseErrorBundle, withRecovery)
import Text.Megaparsec.Char (char)
import Data.Void (Void)
import Data.Text (Text)
import Data.Maybe (isJust)

type Parser a = Parsec Void Text a

type SpecifierParser = Parsec Void Text Specifier
type FormatParser = Parsec Void Text Formatter
type FormatParseError = ParseErrorBundle Text Void


parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe p = withRecovery (const $ return Nothing) (Just <$> p)

parseSingleFlag :: Char -> Parser Bool
parseSingleFlag c = isJust <$> parseMaybe (char c)


{-
makeFormatterParser :: SpecifierParser -> Parser Text -> FormatParser
makeFormatterParser specParser  = foldFormatter $ someaa (specParser <|> litParser)

foldFormatter :: [FormatterF a -> Formatter] -> Formatter
foldFormatter = foldr _ (End :: FormatterF Formatter)
-}

