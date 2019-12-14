module Text.Format.Formatter where

import Text.Format.Specifier

import Data.Functor.Foldable (Fix)
import Data.Text (Text)

data FormatterF a = End | Literal Text a | Spec Specifier a
type Formatter = Fix FormatterF

