# th-text-format

## Example Usage

Some initial examples to provide motivation.

```hs
module Hello where

import Text.Format (sprintf, tprintf, sformat, tformat)

main :: IO ()
main = do
  putStrLn          $ [sprintf|%s-style format strings as %s. Example: %04.2f] "C" "Prelude.String" pi
  putStrLn . unpack $ [tprintf|%s-style format strings as %s. Example: %04.2f] "C" "Data.Text" pi
  putStrLn          $ let lang = "Python" in [sformat|{lang:s}-style format strings as {0:s}. Example: {1:04.2f}] "Prelude.String" pi
  putStrLn . unpack $ let lang = "Python" in [tformat|{lang:s}-style format strings as {0:s}. Example: {1:04.2f}] "Data.Text" pi
```

Things to note:

* The result type of these quasiquoter values is a function of a number of arguments depending on the content of the format string. This provides compile-time type safety against providing too few, too many, or incorrectly-typed values.
* Dictionary-style parameters from Python have access to names bound at the point of the quote expression.
* C-style format strings have an obvious way of counting parameters (one per unescaped '%') but Python format strings could have fewer than the number of format specifiers (for instance, if two specifiers appear with the same index; or if all of the specifiers refer to bound names).

## Design Priorities

* Type safety
    * Parameter types should be overloaded as appropriate:
        * conversion specifiers (or equivalent) 'd', 'i', 'o', 'x', 'u' take a parameter `Integral a => a`.
        * the specifiers 'f', 'e', 'a', 'g' take a parameter `Real a => a`
        * the specifier 's' takes a parameter `IsString s => s`
        * the specifier 'c' takes a character of type `Char`
* Naturalness
    * The format strings should not have any surprising behavior to a programmer already familiar with C or Python format strings
    * Their usage should be as Haskelly as possible, modulo the fact that it involves a language extension
    * To my knowledge, these first two goals taken together require the use of Template Haskell to achieve maximal compile-time safety.
* Nice errors
    * Reported errors should be as helpful as possible; in particular, user-facing errors should never require knowledge of Template Haskell.
* Minimal overhead
    * Some overhead will be inevitable due to the use of Template Haskell and for obvious scenarios like the presence of very long strings. Care should be taken to prevent avoidable leaks, but never at the expense of the above three goals.

## Supported Format Formats

### printf

This format is an emulation of the ANSI C18 `sprintf` format string [specification](https://en.cppreference.com/w/c/io/fprintf), adapted as appropriate for the Haskell environment. Specifically:

(todo: reproduce the printf format spec, taking care to note differences)

* `sprintf` - returns `Prelude.String` (but '%s' parameters can be any IsString instance)
* `tprintf` - returns strict `Data.Text` (ditto)
* `printf` - equivalent to `putStrLn . unpack $ [tprintf|blah|] a b c`

### format

This format is an emulation of Python's [PEP-3101](https://www.python.org/dev/peps/pep-3101/) format strings, adapted as appropriate for the Haskell environment. Specifically:

(todo: reproduce the pep-3101 format spec, taking care to note the differences)

* `sformat` - returns `Prelude.String` (ditto about IsString)
* `tformat` - returns strict `Data.Text` (ditto)
* `format` - equivalent to `putStrLn . unpack $ [tformat|blah|] a b c`
