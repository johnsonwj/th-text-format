module Text.Format.Parsers.Python where

import Text.Format.Parsers.Common
import Text.Format.Specifier

import Control.Applicative ((<|>), liftA2)
import Data.Functor (($>))
import Data.Text (Text, unpack, cons)
import Language.Haskell.TH (Name, mkName)
import Data.Maybe (isJust)

import Text.Megaparsec (takeWhileP, anySingleBut, choice)
import Text.Megaparsec.Char (lowerChar, char)
import Text.Megaparsec.Char.Lexer (decimal)

pyFormatParser :: FormatParser
pyFormatParser = undefined

data FormatStringChunkPy = LiteralPy Text | ReplacementFieldPy ReplacementField
type FormatStringPy = [FormatStringChunkPy]

data ReplacementField = ReplacementField (Maybe FieldName) (Maybe FormatSpecifier)

parseReplacementField :: Parser ReplacementField
parseReplacementField = liftA2 ReplacementField parseFieldName parseFormatSpecifier

parseFieldName :: Parser (Maybe FieldName)
parseFieldName = parseMaybe ((IndexedName <$> decimal) <|> (BoundName <$> parseBoundName)) where
    parseBoundName = mkName . unpack <$> liftA2 cons lowerChar (takeWhileP Nothing $ not . (`elem` invalidNameChars))
    invalidNameChars = "!:}" :: String

parseFormatSpecifier :: Parser (Maybe FormatSpecifier)
parseFormatSpecifier = do
    conversion <- parseConversion
    (fill, align) <- parseAlign
    sign <- parseSign
    altForm <- parseAltForm
    zp <- parseZeroPadded
    width <- parseMinimumWidth
    sep <- parseGrouping
    prec <- parsePrecision

    (actualAlign, ftype) <- parseType conversion fill align sign altForm zp sep
    
    parseMaybe $ char ':' $> FormatSpecifier actualAlign width prec ftype

    where
        -- note this is slightly different than a plain `parseMaybe`; we want to successfully parse Nothing if there is
        -- no '!', but we want to _fail_ if there is a '!' followed by anything other than 's', 'r', or 'a'.
        -- by contrast, parseMaybe (char '!' *> oneOf ['s', 'r', 'a']) would successfully parse `Nothing` for "!b", which
        -- is not correct.
        parseConversion :: Parser (Maybe ConversionFlag)
        parseConversion = parseSingleFlag '!' >>= \hasCon -> if hasCon
            then Just <$> choice conversionFlagParsers
            else return Nothing

        parseAlign :: Parser (Maybe Char, Maybe UntypedAlignFlag)
        parseAlign = parseFillAlign <|> ((Nothing,) <$> parseMaybe parseAlignOnly)

        parseFillAlign :: Parser (Maybe Char, Maybe UntypedAlignFlag)
        parseFillAlign = do
            fillChar <- anySingleBut '}'
            align <- parseAlignOnly
            return (Just fillChar, Just align)

        parseAlignOnly :: Parser UntypedAlignFlag
        parseAlignOnly = choice untypedAlignParsers

        parseSign :: Parser (Maybe SignFlag)
        parseSign = parseMaybe $ choice signFlagParsers

        parseAltForm :: Parser Bool
        parseAltForm = parseSingleFlag '#'

        parseZeroPadded :: Parser Bool
        parseZeroPadded = parseSingleFlag '0'

        parseMinimumWidth :: Parser (Maybe Int)
        parseMinimumWidth = parseMaybe decimal

        parseGrouping :: Parser (Maybe GroupingFlag)
        parseGrouping = parseMaybe ((char '_' $> GroupUnderscore) <|> (char ',' $> GroupComma))

        parsePrecision :: Parser (Maybe Int)
        parsePrecision = parseMaybe (char '.' *> decimal)

        parseType :: Maybe ConversionFlag -> Maybe Char -> Maybe UntypedAlignFlag -> Maybe SignFlag -> Bool -> Bool -> Maybe GroupingFlag -> Parser (Maybe Align, Maybe FieldType)
        parseType cf pad algn sgn alt zp grp = let algn' = algn >>= convertAlign pad in parseMaybe (anySingleBut '}') >>= \case
            Nothing -> banNumericFields algn sgn alt zp grp *> return (algn', Nothing)
            Just 's' -> banNumericFields algn sgn alt zp grp *> return (algn', Just $ strField cf)
            Just 'b' -> return (algn >>= )

        unsafeParseType :: Maybe SignFlag -> 
            

        -- fails the parse if any fields are present which are only valid for numeric types
        banNumericFields :: Maybe UntypedAlignFlag -> Maybe SignFlag -> Bool -> Bool -> Maybe GroupingFlag -> Parser ()
        banNumericFields (Just AlignBetween) _ _ _ _ = numericFieldsDisallowed
        banNumericFields _ Nothing False False Nothing = return ()
        banNumericFields _ _ _ _ _ = numericFieldsDisallowed

numericFieldsDisallowed :: Parser ()
numericFieldsDisallowed = fail "numeric fields are disallowed for this presentation type"

data FieldName = IndexedName Int | BoundName Name

-- The type for this looks a little hinky, because in a proper God-fearing language
-- we want the type to only be able to represent valid values. Unfortunately, Python
-- is not one of those languages, and arbitrary strings less so; there are several
-- ways a format specifier may be considered invalid, and some of those will be
-- accepted by Python anyway. Long story short, the FormatSpecifier type is designed
-- to not support any value which PEP3101 describes as invalid.

data FormatSpecifier = FormatSpecifier
    { fsAlign :: Maybe Align
    , fsMinimumWidth :: Maybe Int
    , fsPrecision :: Maybe Int
    , fsType :: Maybe FieldType
    }

-- note: python allows a 'shortcut', putting a '0' character in front of the width
-- which is equivalent to an align of '0='. They don't specify this in PEP3101, but
-- if both the '0' flag and a conflicting 'align' are provided, the align wins.
data Align = Align { alignFill :: Maybe Char, alignFlag :: AlignFlag }
data AlignFlag = AlignLeft | AlignRight | AlignCentered

-- this is an intermediate type, purely keeping track of the provided align type
-- so the parser can reject a non-numeric field type using AlignBetween ('=')
data UntypedAlignFlag = TypedAlign AlignFlag | AlignBetween

convertAlign :: Maybe Char -> UntypedAlignFlag -> Maybe Align
convertAlign _ AlignBetween = Nothing
convertAlign fill (TypedAlign af) = Just $ Align fill af

untypedAlignParsers :: [Parser UntypedAlignFlag]
untypedAlignParsers =
    [ char '<' $> TypedAlign AlignLeft
    , char '^' $> TypedAlign AlignCentered
    , char '>' $> TypedAlign AlignRight
    , char '=' $> AlignBetween
    ]

data ConversionFlag = ConStr | ConRepr | ConAscii

conversionFlagParsers :: [Parser ConversionFlag]
conversionFlagParsers =
    [ char 's' $> ConStr
    , char 'r' $> ConRepr
    , char 'a' $> ConAscii
    ]

data SignFlag = SignPlus | SignMinus | SignSpace

signFlagParsers :: [Parser SignFlag]
signFlagParsers =
    [ char '+' $> SignPlus
    , char '-' $> SignMinus
    , char ' ' $> SignSpace
    ]

convertSignFlag :: SignFlag -> Maybe PositiveSign
convertSignFlag SignPlus = Just PositivePlus 
convertSignFlag SignMinus = Nothing 
convertSignFlag SignSpace = Just PositiveSpace

data GroupingFlag = GroupUnderscore | GroupComma

data FieldType
    -- first bool: whether or not to put padding between the sign character (if present) and the digits. this overrides the 'AlignFlag'.
    -- second bool: 'alternate form' includes e.g. '0o' or '0x' prefix for non-decimal formats. ignored for decimal.
    = NumField Bool Bool (Maybe SignFlag) NumType

    -- StrField's bool flag is whether or not to accept any `Show` as opposed to an `IsString`
    | StrField Bool

strField :: Maybe ConversionFlag -> FieldType
strField = StrField . isJust

data NumType
    = DecimalNum (Maybe DecimalFlag)
    | FloatingNum (Maybe FloatingFlag)

-- DecimalF and GenF's bool flag is whether or not to use locale-dependent ','/'.' separators
data DecimalFlag = BinaryF | CharF | DecimalF Bool | OctalF | HexF Capitalization 
data FloatingFlag = ExpF Capitalization | FixedF | GenF Bool Capitalization | PercentF

getNumType :: Char -> Maybe NumType
getNumType 'b' = DecimalNum 
