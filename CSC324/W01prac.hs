{-|
Module:        W01prac
Description:   Week 1 Practical: Introduction to Racket and Haskell
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2021

In this part of the exercise, you'll get started writing some simple
functions in Racket. Since this is likely your first time using Racket,
we strongly recommend going through some of the documentation we listed
under the "Software" page as you work through this exercise.
In comments below, we also give some links to documentation to built-in
functions for standard data types (numbers, strings, lists) that we want
you to become familiar with.

Finally, you'll notice the (module+ test ...) expressions interleaved with
the function definitions; this is a standard Racket convention for simple
unit tests that we'll use throughout the course. Please read them carefully,
and add tests of your own!
-}

-- This lists what this module exports. Don't change this!
module W01prac
  (
    celsiusToFahrenheit
  , nCopies
  , appears
  )
where

import Test.QuickCheck (Property, quickCheck, (==>))

-------------------------------------------------------------------------------
-- * Note about type signatures
--
-- Unlike Racket, Haskell is /statically-typed/. We'll go into more detail about
-- what this means later in the course, but for now we've provided type signatures
-- for the functions here to simplify any compiler error messages you might
-- receive. (Don't change them; they're required to compile against our tests.)
-------------------------------------------------------------------------------

-- | Converts a temperature from Celsius to Fahrenheit.
-- __Note__: use the @round@ function to convert from floating-point types
-- to @Int@.
celsiusToFahrenheit :: Float -> Int
celsiusToFahrenheit temp =
    -- TODO: replace `undefined` with a proper function body.
    round ((temp * 9/5) + 32)

-- | The simplest "property-based test" is simply a unit test; note the type.
prop_celsius0 :: Bool
prop_celsius0 = celsiusToFahrenheit 0 == 32
prop_celsius37 :: Bool
prop_celsius37 = celsiusToFahrenheit 37 == 99

-------------------------------------------------------------------------------
-- * Recursion with numbers
--
-- For the recursive functions, we recommend doing these in two ways:
--
--   1. First, write them using @if@ expressions, as you would in Racket.
--   2. Then when that works, use /pattern-matching/ to simplify the definitions
--      (<http://learnyouahaskell.com/syntax-in-functions#pattern-matching>).
--
-- Remember: Strings are simply lists of characters. (@String === [Char]@)
-- Read more about manipulating lists at
-- <http://learnyouahaskell.com/starting-out#an-intro-to-lists>.

-- | Returns a new string that contains @n@ copies of the input string.
nCopies :: String -> Int -> String
nCopies s n =
  if n == 0 then ""
  else s ++ nCopies s (n-1) 


-- | This is a QuickCheck property that says,
-- "If n >= 0, then when you call nCopies on a string s and int n,
-- the length of the resulting string is equal to
-- n * the length of the original string."
--
-- QuickCheck verifies this property holds for a random selection of
-- inputs (by default, choosing 100 different inputs).
prop_nCopiesLength :: String -> Int -> Property
prop_nCopiesLength s n = n >= 0 ==> length (nCopies s n) == (length s * n)

-------------------------------------------------------------------------------
-- * Recursion with lists
-------------------------------------------------------------------------------

-- | Returns whether a string appears in a list of strings
--
-- We've given you a recursive template here to start from.
-- But noted as above, you can later try simplifying this definition
-- using pattern matching.
appears :: String -> [String] -> Bool
appears s lst = if null lst
  then False
  else (head lst) == s || appears s (tail lst)
--     ...
--   else
--     let firstVal = head lst
--         stringAppearInRest = appears (tail numbers)
--     in
--        ...

-- | This is a QuickCheck property that says,
-- "When you call appears on the string s and the list [s, s]
--  you should get True"
--
-- QuickCheck verifies this property holds for a random selection of
-- inputs (by default, choosing 100 different inputs).
prop_nAppearsTwice :: String -> Bool
prop_nAppearsTwice s = appears s [s, s]

-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
  quickCheck prop_celsius0
  quickCheck prop_celsius37
  quickCheck prop_nCopiesLength
  quickCheck prop_nAppearsTwice
