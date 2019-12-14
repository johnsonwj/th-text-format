module Text.Format.Parsers.Common where

import Text.Format.Formatter

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Text (Text)

type FormatParser = Parsec Void Text Formatter
type FormatParseError = ParseErrorBundle Text Void
