module Text.Format.Quoters where

import Text.Format.Parsers

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

import Data.Text (Text, pack, unpack)

printf, sprintf, tprintf, format, sformat, tformat :: QuasiQuoter
printf  = QuasiQuoter printfExpr  lprintf unsupportedQuoteType unsupportedQuoteDec
sprintf = QuasiQuoter sprintfExpr lprintf unsupportedQuoteType unsupportedQuoteDec
tprintf = QuasiQuoter tprintfExpr lprintf unsupportedQuoteType unsupportedQuoteDec
format  = QuasiQuoter formatExpr  lformat unsupportedQuoteType unsupportedQuoteDec
sformat = QuasiQuoter sformatExpr lformat unsupportedQuoteType unsupportedQuoteDec
tformat = QuasiQuoter tformatExpr lformat unsupportedQuoteType unsupportedQuoteDec

printfExpr, sprintfExpr, tprintfExpr, formatExpr, sformatExpr, tformatExpr :: String -> Q Exp
printfExpr  = qPutStrLn . tprintfExpr
sprintfExpr = qUnpackString . tprintfExpr
tprintfExpr = buildExprQuoter cFormatParser . pack
formatExpr  = qPutStrLn . tformatExpr
sformatExpr = qUnpackString . tformatExpr
tformatExpr = buildExprQuoter pyFormatParser . pack

-- formatting to String literals
lprintf, lformat :: String -> Q Pat
lprintf = undefined
lformat = undefined

unsupportedQuoteType :: String -> Q Type
unsupportedQuoteType = const $ fail "th-text-format does not support formatting type literals yet"

unsupportedQuoteDec :: String -> Q [Dec]
unsupportedQuoteDec = const $ fail "th-text-format cannot generate a top-level declaration...?"

buildExprQuoter :: FormatParser -> Text -> Q Exp
buildExprQuoter = undefined

buildPatQuoter :: FormatParser -> Text -> Q Pat
buildPatQuoter = undefined

qPutStrLn :: Q Exp -> Q Exp
qPutStrLn tq = tq >>= \case
    LamE ps e   -> LamE ps <$> appE [| (putStrLn . unpack) |] (return e)
    _           -> fail "unexpected non-lambda argument to qPutStrLn ??"

qUnpackString :: Q Exp -> Q Exp
qUnpackString tq = tq >>= \case
    LamE ps e   -> LamE ps <$> appE [| unpack |] (return e)
    _           -> fail "unexpected non-lambda argument to qUnpackString ??"

