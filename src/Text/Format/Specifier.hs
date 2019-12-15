module Text.Format.Specifier where

import Data.Default
-- C99 printf notes:
--   * length modifiers are ignored; they modify behavior based on the size of an integer argument
--     (long, unsigned, etc) which is irrelevant in Haskell
--   * the `p` conversion specifier is taken to mean "the result of `show` for an argument with a Show instance"
--   * arguments to an `s` parameter are instances of IsString
--   * the `n` conversion specifier is ignored; it is a pointer that `printf` *writes to*, which doesn't make sense here

data Specifier = Specifier 
    { fsJustify :: Justification
    , fsFieldWidth :: Maybe Length
    , fsPrecision :: Maybe Length
    , fsConversion :: Conversion
    } deriving (Show, Eq)

data Justification = JustifyRight Padding | JustifyLeft deriving (Show, Eq)

instance Default Justification where
    def = JustifyRight def

data Padding = PadSpace | PadZero deriving (Show, Eq)
instance Default Padding where
    def = PadSpace

data Length = ParameterizedLength | LiteralLength Int deriving (Show, Eq)

defaultPrecision :: Conversion -> Maybe Length
defaultPrecision FloatingHexC {} = Nothing -- this one is somewhat arcane
defaultPrecision c
    | isIntegralArg c = Just (LiteralLength 1)
    | isFloatingArg c = Just (LiteralLength 6)
    | otherwise = Nothing

data Conversion
    -- DecC, OctC, HexC: arg is an Integral
    = DecC PositiveSign
    | OctC OctForm PositiveSign
    | HexC HexForm Capitalization PositiveSign
    -- Floating*: arg is a Real
    | FloatingDecC DecimalPoint Capitalization PositiveSign
    | FloatingExpC DecimalPoint Capitalization PositiveSign
    | FloatingGenC DecimalPoint Capitalization PositiveSign
    | FloatingHexC DecimalPoint Capitalization PositiveSign
    | CharC -- arg is a Char
    | StringC -- arg is an IsString
    | ShowC -- arg is a Show
    deriving (Show, Eq)

isIntegralArg :: Conversion -> Bool
isIntegralArg DecC {} = True
isIntegralArg OctC {} = True
isIntegralArg HexC {} = True
isIntegralArg _ = False

isFloatingArg :: Conversion -> Bool
isFloatingArg FloatingDecC {} = True
isFloatingArg FloatingExpC {} = True
isFloatingArg FloatingGenC {} = True
isFloatingArg FloatingHexC {} = True
isFloatingArg _ = False

data PositiveSign = PositiveEmpty | PositivePlus | PositiveSpace deriving (Show, Eq)
instance Default PositiveSign where
    def = PositiveEmpty

data Capitalization = Upper | Lower deriving (Show, Eq) -- whether to capitalize "e", "inf", "x", etc.

data OctForm = UnprefixedOctal | PrefixZero deriving (Show, Eq) -- PrefixZero will implicitly increase the given precision by one iff necessary
instance Default OctForm where
    def = UnprefixedOctal

data HexForm = Prefix0x | UnprefixedHex deriving (Show, Eq)
instance Default HexForm where
    def = UnprefixedHex

data DecimalPoint = AsNeeded | Always deriving (Show, Eq)
instance Default DecimalPoint where def = AsNeeded
        