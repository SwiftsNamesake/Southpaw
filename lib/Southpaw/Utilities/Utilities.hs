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



module Southpaw.Utilities.Utilities (thousands, abbreviate, chunks, numeral, split, pairwise) where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List (intercalate, unfoldr)



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
-- TODO: Rename (?)
split :: Eq a => a -> [a] -> [[a]]
split c s = unfoldr cut s
  where cut [] = Nothing
        cut xs = let (token, rest) = span (/=c) xs in Just (token, dropWhile (==c) rest)
-- split c s = filter (/=[c]) . groupBy ((==) `on` (==c)) $ s


-- |
pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs $ tail xs


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



---------------------------------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------------------------------
