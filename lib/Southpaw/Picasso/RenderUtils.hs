-- |
-- Module      : RenderUtils
-- Description : Cairo rendering utilities
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created July 12 2015

-- TODO | - 
--        - 

-- SPEC | -
--        -



module Southpaw.Picasso.RenderUtils where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import qualified Graphics.Rendering.Cairo as Cairo
import Data.Complex

import qualified Southpaw.Picasso.Palette as Palette



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- General rendering utilities --------------------------------------------------------------------
-- |
-- TODO: General anchor (?)
renderCentredText :: Complex Double -> String -> Cairo.Render ()
renderCentredText (cx:+cy) text = do
	extents <- Cairo.textExtents text
	let (w, h) = (Cairo.textExtentsWidth extents, Cairo.textExtentsHeight extents)
	Cairo.moveTo (cx-w/2) (cy+h/2)
	Cairo.showText text


-- | Choose a colour
choose :: Palette.Colour Double -> Cairo.Render ()
choose (r, g, b, a) = Cairo.setSourceRGBA r g b a


-- Vector utilities -------------------------------------------------------------------------------
-- TODO: Move to another module (?)
-- |
vectorise :: (Double -> Double -> a) -> (Complex Double) -> a
vectorise f (x:+y) = f x y


-- |
flipx :: Complex Double -> Complex Double
flipx (x:+y) = (-x):+y


-- |
flipy :: Complex Double -> Complex Double
flipy (x:+y) = x:+(-y)


-- |
real :: Double -> Complex Double
real = (:+ 0)


-- |
imag :: Double -> Complex Double
imag = (0 :+)


---------------------------------------------------------------------------------------------------
-- | 
-- TODO: Unsafe, use Maybe (?)
closePath :: [Complex Double] -> [Complex Double]
closePath path = path ++ [head path]


-- Control structures -----------------------------------------------------------------------------
-- |
grid :: (Integral n, Enum n) => n -> n -> (n -> n -> a) -> [a] 
grid cols rows f = [ f cl rw | cl <- [0..(cols-1)], rw <- [0..(rows-1)] ] -- Tiles


-- | 
-- TODO: Rename (eg. something pertaining to 2D loops)
-- TODO: Underscores, grrr
gridM_ :: (Integral n, Enum n, Monad m) => n -> n -> (n -> n -> m a) -> m ()
gridM_ cols rows f = sequence_ $ grid cols rows f

