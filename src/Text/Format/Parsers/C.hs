module Text.Format.Parsers.C where

import Text.Format.Parsers.Common
import Text.Format.Specifier

import Data.Text (Text, pack)
import Data.String (IsString(..))
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Control.Applicative.Combinators (many, choice)

import Text.Megaparsec (takeWhile1P, withRecovery)
import Text.Megaparsec.Char (string, char)
import Text.Megaparsec.Char.Lexer (decimal)

cFormatParser :: FormatParser
cFormatParser = undefined

data FormatStringChunkC = LiteralC Text | SpecifierC ConversionSpecC deriving (Show, Eq)
type FormatStringC = [FormatStringChunkC]

instance IsString FormatStringChunkC where
    fromString = LiteralC . pack

cFormatStringParser :: Parser FormatStringC
cFormatStringParser = many ((LiteralC <$> litParserC) <|> (SpecifierC <$> parseConversionSpecC))

litParserC :: Parser Text
litParserC = (string "%%" $> "%") <|> takeWhile1P Nothing (/= '%')

data ConversionSpecC = ConversionSpecC
    { ccFlags :: [Flag]
    , ccFieldWidth :: Maybe Length
    , ccPrecision :: Maybe Length
    , ccLengthModifier :: Maybe LengthModifier
    , ccConversionSpecifier :: ConversionSpecifierC
    } deriving (Show, Eq)

-- todo: give this a `Show` instance that prints the corresponding format string

data Flag
    = CJustifyLeft
    | CPositivePlus
    | CPositiveSpace
    | CAlternateForm
    | CZeroPadding
    deriving (Show, Eq)

data LengthModifier
    = CharL
    | ShortL
    | LongL
    | LongLongL
    | IntMaxL
    | SizeL
    | PtrDiffL
    | LongDoubleL
    deriving (Show, Eq)

data ConversionSpecifierC
    = CIntegral Signedness IntBase Capitalization
    | CFloatingDec Capitalization
    | CFloatingExp Capitalization
    | CFloatingGen Capitalization
    | CFloatingHex Capitalization
    | CChar
    | CString
    | CPtr
    deriving (Show, Eq)

data Signedness = Signed | Unsigned deriving (Show, Eq)
data IntBase = Dec | Oct | Hex deriving (Show, Eq)

cToCommon :: ConversionSpecC -> Specifier
cToCommon = undefined

parseConversionSpecC :: Parser ConversionSpecC
parseConversionSpecC = char '%' $> ConversionSpecC <*> parseFlags <*> parseFieldWidth <*> parsePrecision <*> parseLengthModifier <*> parseConversionSpecifier where

    parseFlags :: Parser [Flag]
    parseFlags = many $ choice flagParsers

    parseFieldWidth :: Parser (Maybe Length)
    parseFieldWidth = parseMaybe parseLength

    parsePrecision :: Parser (Maybe Length)
    parsePrecision = parseMaybe (char '.' *> withRecovery (const . return $ LiteralLength 0) parseLength)

    parseLengthModifier :: Parser (Maybe LengthModifier)
    parseLengthModifier = parseMaybe $ choice lengthModifierParsers

    parseConversionSpecifier :: Parser ConversionSpecifierC
    parseConversionSpecifier = choice conversionSpecifierParsers

flagParsers :: [Parser Flag]
flagParsers =
    [ char '-' $> CJustifyLeft
    , char '+' $> CPositivePlus
    , char ' ' $> CPositiveSpace
    , char '#' $> CAlternateForm
    , char '0' $> CZeroPadding
    ]

lengthModifierParsers :: [Parser LengthModifier]
lengthModifierParsers =
    [ string "hh" $> CharL
    , char 'h' $> ShortL
    , string "ll" $> LongLongL
    , char 'l' $> LongL
    , char 'j' $> IntMaxL
    , char 'z' $> SizeL
    , char 't' $> PtrDiffL
    , char 'L' $> LongDoubleL
    ]

conversionSpecifierParsers :: [Parser ConversionSpecifierC]
conversionSpecifierParsers =
    [ (char 'd' <|> char 'i') $> CIntegral Signed Dec Upper
    , char 'o' $> CIntegral Unsigned Oct Upper
    , char 'u' $> CIntegral Unsigned Dec Upper
    , char 'x' $> CIntegral Unsigned Hex Lower
    , char 'X' $> CIntegral Unsigned Hex Upper 
    , char 'f' $> CFloatingDec Lower
    , char 'F' $> CFloatingDec Upper
    , char 'e' $> CFloatingExp Lower
    , char 'E' $> CFloatingExp Upper
    , char 'g' $> CFloatingGen Lower
    , char 'G' $> CFloatingGen Upper
    , char 'a' $> CFloatingHex Lower
    , char 'A' $> CFloatingHex Upper
    , char 'c' $> CChar
    , char 's' $> CString
    , char 'p' $> CPtr
    ]

parseLength :: Parser Length
parseLength = char '*' $> ParameterizedLength <|> LiteralLength <$> decimal
