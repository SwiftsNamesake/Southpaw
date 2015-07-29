--
-- Utilities.hs
-- ?
--
-- Jonatan H Sundqvist
-- March 08 2015
--

-- TODO | - 
--        - 

-- SPEC | -
--        -
-- |
-- Module      : Southpaw.Utilities.Utilities
-- Description : General functional bits and bobs
-- Copyright   : (c) Jonatan H Sundqvist, year
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



module Southpaw.Utilities.Utilities (thousands, abbreviate, chunks, numeral, split, pairwise, cuts, count, gridM) where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List     (intercalate, unfoldr)
import Control.Monad (forM, forM_)


---------------------------------------------------------------------------------------------------
-- Section
---------------------------------------------------------------------------------------------------
-- Grammar and string formatting ------------------------------------------------------------------
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
pairwise f xs = zipWith f xs $ drop 1 xs


-- Verb conjugation
{-    where be 1 = "is"
          be _ = "are"
          suffix 1  = ""
          suffix _  = "s"
-}


-- | Converts a positive integer to an English numeral. Numbers above twelve are converted to comma-separated strings
--
-- TODO: 
--
numeral :: Int -> String
numeral n = case n of
  0  -> "zero"
  1  -> "one"
  2  -> "two"
  3  -> "three"
  4  -> "four"
  5  -> "five"
  6  -> "six"
  7  -> "seven"
  8  -> "eight"
  9  -> "nine"
  10 -> "ten"
  11 -> "eleven"
  12 -> "twelve"
  _  -> thousands (n :: Int)


-- General utilities ------------------------------------------------------------------------------
-- | Counts the number of elements that satisfy the predicate
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p


-- Control structures -----------------------------------------------------------------------------
-- |
grid :: (Integral n) => n -> n -> [(n, n)]
grid cols rows = [ (col, row) | col <- [1..cols], row <- [1..rows]]


-- |
gridM :: (Integral n, Monad m) => n -> n -> (n -> n -> m a) -> m [a]
gridM cols rows f = forM (grid cols rows) (uncurry f)


-- |
gridM_ :: (Integral n, Monad m) => n -> n -> (n -> n -> m a) -> m ()
gridM_ cols rows f = forM_ (grid cols rows) (uncurry f)


-- Experiments ------------------------------------------------------------------------------------
-- | Polymorphism with list of records of functions



---------------------------------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------------------------------
