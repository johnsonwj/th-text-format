module CParserSpec where

import Test.Hspec
import Text.Format.Parsers.C
import Text.Format.Specifier
import Text.Megaparsec (parse)
import Data.Text (Text)

spec :: Spec
spec = do
    it "parses a literal string" $ checkParse "abcd" ["abcd"]
    it "parses a literal starting with an escaped %" $ checkParse "%%abcd" ["%", "abcd"]
    it "parses a literal ending with an escaped %" $ checkParse "abcd%%" ["abcd", "%"]
    it "parses a literal with an escaped % in the middle" $ checkParse "ab%%cd" ["ab", "%", "cd"]
    it "parses a single literal %" $ checkParse "%%" ["%"]

    it "parses a plain specifier" $ checkParse "%s" [stringSpecifier]
    -- todo: more; also, automate?

    it "parses a literal followed by a specifier" $ checkParse "abcd%.2f" ["abcd", prec2Float]
    it "parses a literal starting with a specifier" $ checkParse "%.2fabcd" [prec2Float, "abcd"]
    it "parses a literal containing a specifier" $ checkParse "ab%.2fcd" ["ab", prec2Float, "cd"]
    it "parses a literal containing a specifier immediately after a literal %" $ checkParse "ab%%%.2fcd" ["ab", "%", prec2Float, "cd"]
    it "parses a literal containing a specifier immediately before a literal %" $ checkParse "ab%.2f%%cd" ["ab", prec2Float, "%", "cd"]

    
checkParse :: Text -> FormatStringC -> Expectation
checkParse fs expected = parse cFormatStringParser "" fs `shouldBe` Right expected

stringSpecifier :: FormatStringChunkC
stringSpecifier = SpecifierC (ConversionSpecC [] Nothing Nothing Nothing CString)

prec2Float :: FormatStringChunkC
prec2Float = SpecifierC (ConversionSpecC [] Nothing (Just $ LiteralLength 2) Nothing (CFloatingDec Lower))

