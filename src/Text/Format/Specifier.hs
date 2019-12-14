module Text.Format.Specifier where

import Data.Default
-- C99 printf notes:
--   * length modifiers are ignored; they modify behavior based on the size of an integer argument
--     (long, unsigned, etc) which is irrelevant in Haskell
--   * the `p` conversion specifier is taken to mean "the result of `show` for an argument with a Show instance"
--   * arguments to an `s` parameter are instances of IsString
--   * C99 specifies that `n` consumes but does not perform any conversion on an argument. This is silly, so in this
--     implementation, no argument will be generated for an `n`.

data Specifier = Specifier 
    { fsJustify :: Justification
    , fsFieldWidth :: Maybe Length
    , fsPrecision :: Maybe Length
    , fsConversion :: Conversion
    }

data Justification = JustifyRight Padding | JustifyLeft

instance Default Justification where
    def = JustifyRight def

data Padding = PadSpace | PadZero
instance Default Padding where
    def = PadSpace

data Length = ParameterizedLength | LiteralLength Int

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
    | CountC -- no arg

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

data PositiveSign = PositiveEmpty | PositivePlus | PositiveSpace
instance Default PositiveSign where
    def = PositiveEmpty

data Capitalization = Upper | Lower -- whether to capitalize "e", "inf", "x", etc.

data OctForm = UnprefixedOctal | PrefixZero -- PrefixZero will implicitly increase the given precision by one iff necessary
instance Default OctForm where
    def = UnprefixedOctal

data HexForm = Prefix0x | UnprefixedHex
instance Default HexForm where
    def = UnprefixedHex

data DecimalPoint = AsNeeded | Always
instance Default DecimalPoint where def = AsNeeded
        