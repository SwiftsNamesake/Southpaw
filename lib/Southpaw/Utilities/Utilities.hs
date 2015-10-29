-- |
-- Module      : Southpaw.Utilities.Utilities
-- Description : General functional bits and bobs
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created March 08 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC directives
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
-- TODO: Organise exports (by category, haddock headers)
module Southpaw.Utilities.Utilities (thousands, abbreviate, numeral, roman,
                                     chunks, split, pairwise, cuts, count,
                                     gridM, degrees,
                                     radians, π) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.List     (intercalate, unfoldr)
import Control.Monad (forM, forM_)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Section
--------------------------------------------------------------------------------------------------------------------------------------------
-- Grammar and string formatting -----------------------------------------------------------------------------------------------------------
-- | Format a number with thousand separators
-- TODO: Allow non-integral numbers
thousands :: Int -> String
thousands = reverse . intercalate "," . chunks 3 . reverse . show

-- |
-- TODO: Optional ellipsis argument
-- TODO: Better name (?)
-- let len = length s in take (min len $ n-3) s ++ take () "..."
abbreviate :: Int -> String -> String
abbreviate n s
    | n < length s = let visible = n-3 in take visible s ++ "..."
    | otherwise    = s


-- | Divides a list into chunks of the given size
--
-- assert chunks 5 "fivesknifelives" == ["fives", "knife", "lives"] -- TODO: Move this to a test section
--
-- TODO: Implement with higher-order recursive function (cf. foldr, iterate, until) (?)
--
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (chunk, rest) = splitAt n xs in chunk : chunks n rest
-- chunks n = unfoldr (\ xs -> if null xs then Nothing else splitAt n xs)


-- |
splitWith :: Eq a => ([a] -> ([a], [a])) -> [a] -> [[a]]
splitWith f = unfoldr cut
  where cut [] = Nothing
        cut xs = Just $ f xs


-- |
-- TODO: Rename (?)
split :: Eq a => a -> [a] -> [[a]]
split c = splitWith $ \ xs -> let (token, rest) = span (/=c) xs in (token, dropWhile (==c) rest)
-- split c s = filter (/=[c]) . groupBy ((==) `on` (==c)) $ s


-- | Same as Python's str.split (I think) (eg. split '|' "a||c" == ["a", "", "c"])
-- TODO: Rename
-- TODO: Easier to implement with groupBy (?)
cuts :: Eq a => a -> [a] -> [[a]]
cuts c = splitWith $ \ xs -> let (token, rest) = span (/=c) xs in (token, drop 1 rest)


-- |
-- TODO: Accept a function (eg. (a -> a -> b))
pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise f xs = zipWith f xs (drop 1 xs)


-- Verb conjugation
{-    where be 1 = "is"
          be _ = "are"
          suffix 1  = ""
          suffix _  = "s"
-}


-- | Converts a positive integer to an English numeral. Numbers above twelve are converted to comma-separated strings
--
-- TODO: Refactor, simplify (?)
--
numeral :: Int -> String
numeral n = case n of
  0  -> sign "zero"
  1  -> sign "one"
  2  -> sign "two"
  3  -> sign "three"
  4  -> sign "four"
  5  -> sign "five"
  6  -> sign "six"
  7  -> sign "seven"
  8  -> sign "eight"
  9  -> sign "nine"
  10 -> sign "ten"
  11 -> sign "eleven"
  12 -> sign "twelve"
  _  -> thousands (n :: Int)
  where
    sign | n < 0     = ("negative " ++) -- TODO: Use 'minus' instead, optional (?)
         | otherwise = (id)


-- |
-- TODO: Finish
roman :: Int -> Maybe Char
roman n = case n of
  0    -> Nothing
  1    -> Just 'I'
  5    -> Just 'V'
  10   -> Just 'X'
  50   -> Just 'L'
  100  -> Just 'C'
  500  -> Just 'D'
  1000 -> Just 'M'



-- General utilities -----------------------------------------------------------------------------------------------------------------------
-- | Counts the number of elements that satisfy the predicate
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p


-- Control structures ----------------------------------------------------------------------------------------------------------------------
-- |
grid :: (Integral n) => n -> n -> [(n, n)]
grid cols rows = [ (col, row) | col <- [1..cols], row <- [1..rows]]


-- |
gridM :: (Integral n, Monad m) => n -> n -> (n -> n -> m a) -> m [a]
gridM cols rows f = forM (grid cols rows) (uncurry f)


-- |
gridM_ :: (Integral n, Monad m) => n -> n -> (n -> n -> m a) -> m ()
gridM_ cols rows f = forM_ (grid cols rows) (uncurry f)


-- Math ------------------------------------------------------------------------------------------------------------------------------------
--
π :: Floating a => a
π = pi


-- |
-- TODO: Unit types, Num instance (?)
-- TODO: Rename (eg. 'toDegrees', 'fromRadians') (?)
degrees :: Floating f => f -> f
degrees rad = rad * (180.0/π)

-- |
radians :: Floating f => f -> f
radians deg = deg * (π/180.0)


-- Experiments -----------------------------------------------------------------------------------------------------------------------------
-- | Polymorphism with list of records of functions



--------------------------------------------------------------------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------------------------------------------------------------------
