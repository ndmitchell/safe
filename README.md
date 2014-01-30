# Safe [![Build Status](https://travis-ci.org/ndmitchell/safe.png)](https://travis-ci.org/ndmitchell/safe)

A library for increasing the safety of your code, by wrapping Prelude functions that potentially crash. These functions can be used to reduce the number of unsafe pattern matches in a program.

Each unsafe function has up to four additional forms. For example, with `tail`:

* <tt>tail :: [a] -> [a]</tt>, crashes on `tail []`
* <tt>tailNote :: <i>String</i> -> [a] -> [a]</tt>, takes an extra argument which supplements the error message
* <tt>tailDef :: <i>[a]</i> -> [a] -> [a]</tt>, takes a default to return on errors
* <tt>tailMay :: [a] -> <i>Maybe</i> [a]</tt>, wraps the result in a `Maybe`
* <tt>tailSafe :: [a] -> [a]</tt>, returns some sensible default if possible, `[]` in the case of `tail`

This library also introduces three brand new functions:

* `at`, a synonym for `(!!)`
* `lookupJust`, defined as `lookupJust k = fromJust . lookup k`
* `abort`, same as `error`, but for a deliberate and intentional program abort
