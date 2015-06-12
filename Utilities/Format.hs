--
-- Format.hs
-- Experimental printf-style formatting
--
-- Jonatan H Sundqvist
-- February 14 2015
--

-- TODO | -
--        -

-- SPEC | -
--        -



{-# LANGUAGE FlexibleInstances #-}



---------------------------------------------------------------------------------------------------
-- Text formatting
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Rename (?)
newtype FShow a = FShow a


-- |
-- TODO: Rename (?)
class Formattable a where
	interpolate :: String -> a -> [Char]


-- |
-- TODO: Rename (?)
instance Show a => Formattable (FShow a) where
	interpolate f (FShow a) = show a


instance (Formattable a, Formattable b) => Formattable (a -> b) where
	interpolate f a = "Hello"


instance Formattable a => Formattable (a -> [Char]) where
	interpolate f a = "Hello"



format :: Formattable a => String -> a
format f a = interpolate f a



---------------------------------------------------------------------------------------------------
-- Section
---------------------------------------------------------------------------------------------------
-- data Tuple a b = Head a |



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	putStrLn ""
	putStrLn ""