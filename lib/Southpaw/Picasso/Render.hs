-- |
-- Module      : Southpaw.Picasso.Render
-- Description : Vector rendering in 2D with Cairo
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 1 2015

-- TODO | - Rename (eg. Draw, Shapes) (?)
--        - Text, typography (find good library)
--        - Debug versions (cf. Occlusion.Render)
--        - Use BoundingBox (?)

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC directives
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Southpaw.Picasso.Render where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Control.Monad (forM, forM_, liftM, liftM2, void)

import Control.Monad.IO.Class

import qualified Graphics.Rendering.Cairo                         as Cairo
-- import qualified Graphics.Rendering.Cairo.Internal.Surfaces.Image as Image

import qualified Southpaw.Picasso.Palette     as Palette
import qualified Southpaw.Picasso.Shapes      as Shapes
import           Southpaw.Picasso.RenderUtils
import           Southpaw.Math.Constants
import           Southpaw.Cartesian.Plane.Utilities



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- General ---------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Rename (eg. plot)
-- TODO: Generalise (not just circles)
trail :: Palette.Colour Double -> [Complex Double] -> Cairo.Render ()
trail fill trail = forM_ trail $ \dot -> do
    choose fill
    circle dot 3


-- |
grid :: Int -> Int -> Double -> Cairo.Render ()
grid cols rows size = do
    -- TODO: Figure out how to use fill AND stroke
    Cairo.setLineWidth 4
    gridM_ cols rows $ \ cl rw -> tilePath cl rw >> Cairo.fill   --
    gridM_ cols rows $ \ cl rw -> tilePath cl rw >> Cairo.stroke --
    where
      chooseColour cl rw = if (cl `mod` 2) == (rw `mod` 2) then 0.3 else 0.75 -- TODO: This should be a utility function
      tilePath cl rw     = Cairo.rectangle (fromIntegral cl*size) (fromIntegral rw*size) size size >> Cairo.setSourceRGBA 0.22 0.81 (chooseColour cl rw) 0.32

-- Primitives ------------------------------------------------------------------------------------------------------------------------------

-- |
line :: Complex Double -> Complex Double -> Cairo.Render ()
line (fr:+om) (t:+o) = Cairo.moveTo fr om >> Cairo.lineTo t o


-- | Renders a path of connected lines
-- TODO: Options for colour, width, closed/open, etcairo.
linepath :: [Complex Double] -> Cairo.Render ()
linepath []      = return ()
linepath (e:dge) = void $ vectorise Cairo.moveTo e >> forM dge (vectorise Cairo.lineTo)


-- |
-- TODO: Support asymmetrical crosshairs (?)
crosshairs :: Complex Double -> Complex Double -> Cairo.Render ()
crosshairs centre size = do
  line (centre - (hdx:+0)) (centre + (hdx:+0))
  line (centre - (0:+hdy)) (centre + (0:+hdy))
  where
    (hdx:+hdy) = 0.5*size

-- Shapes ----------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Extract argument conversion logic (centre/size vectors to unpacked left-top/dx/dy)
rectangle :: Complex Double -> Complex Double -> Cairo.Render ()
rectangle (cx:+cy) (dx:+dy) = Cairo.rectangle (cx-dx/2) (cy-dy/2) dx dy


-- | A rectangle with an 'anchor' (in normalised coordinates) that is relative to the centre
-- TODO: Should anchor point be relative to centre or topleft corner, use normalised or absolute coords (?)
-- TODO: Less confusing terminology...
-- TODO: Refactor
-- TODO: Test
anchoredRectangle :: Complex Double -> Complex Double -> Complex Double -> Cairo.Render ()
anchoredRectangle p size anchor = rectangle (p-dotwise (*) (anchor+(0.5:+0.5)) size) size


-- |
-- TODO: Add arguments for colour, stroke, etcairo.
-- TODO: Maybe it'd be better if we stuck to the normal pattern of path-config-action that Cairo follows
-- TODO: Make polymorphic
polygon :: Integral int => int -> Double -> Complex Double -> (Double, Double, Double, Double) -> Bool -> Cairo.Render ()
polygon sides radius origin (r,g,b,a) filled = do
    -- TODO: Refine 'wrap-around logic'
    Cairo.moveTo fx fy
    forM_ rest $ \(x:+y) -> Cairo.lineTo x y

    Cairo.setSourceRGBA r g b a
    Cairo.setLineWidth 12
    if filled
        then Cairo.fill
        else Cairo.stroke
    where
      ((fx:+fy):rest) = Shapes.polygon sides radius origin ++ [fx:+fy]


-- |
-- TODO: Options for fill/stroke, colour, width, etcairo.
circle :: Complex Double -> Double -> Cairo.Render ()
circle (cx:+cy) radius = do
    Cairo.arc cx cy radius 0 τ
    -- Cairo.fill

-- Composite -------------------------------------------------------------------------------------------------------------------------------

-- |
arrow :: Complex Double -> Complex Double -> Double -> Double -> Double -> Cairo.Render ()
arrow from to sl sw hw = do
    let (first:rest) = closePath $ Shapes.arrow from to sl sw hw
    vectorise Cairo.moveTo first
    forM_ rest $ vectorise Cairo.lineTo


-- |
-- Ugh, I hate underscores so much
-- TODO: Make polymorphic
circlearc :: Int -> Complex Double -> Double -> Double -> Double -> Double -> Cairo.Render ()
circlearc
    count    -- Number of small circles
    (ox:+oy) -- Centre of the 'arc' (pixels?)
    spread   -- Radius of the 'arc'
    radius   -- Radius of the small circles
    begin    -- Start angle of the arc
    extent = forM_ [1..count] $ \ n -> do
        let n' = fromIntegral n
        let θ  = begin + n'*extent/fromIntegral count
        Cairo.arc (ox - spread*cos θ) (oy - spread*sin θ) radius 0 τ
        Cairo.setSourceRGBA (0.5 * (1 + sin θ)) (0.1*n') (1/n') 0.95
        Cairo.fill


-- |
bezier :: Complex Double -> Complex Double -> Complex Double -> Cairo.Render ()
bezier (x1:+y1) (x2:+y2) (x3:+y3) = Cairo.curveTo x1 y1 x2 y2 x3 y3


-- |
-- TODO: Generic rounded polygon
roundrect :: Complex Double -> Complex Double -> Double -> Cairo.Render ()
roundrect centre@(cx:+cy) size@(dx:+dy) radius = forM_ (zip [real, imag, real, imag] [(-dx):+(-dy), (dx):+(-dy), (dx):+(dy), (-dx):+(dy)]) $ \(dir, delta@(dx':+dy')) -> do

  -- TODO: Finish refactoring

	--
	-- let dir = (signum (dx*dy))

	-- Line segment
	vectorise Cairo.moveTo (centre + delta + dir radius)
	vectorise Cairo.lineTo (centre - flipx size/2 - real radius)

	-- Curve

	-- -- First line segment
	-- vectorise Cairo.moveTo (centre - size/2       + real radius)
	-- vectorise Cairo.lineTo (centre - flipx size/2 - real radius)

	-- -- Curve
	-- let (cx':+cy') = (centre - flipx size/2 + ((-radius):+radius)) in Cairo.arc cx' cy' radius (3*π/2) (4*π/2)

	-- -- Second line segment
	-- vectorise Cairo.moveTo (centre - flipx size/2  + imag radius)
	-- vectorise Cairo.lineTo (centre + size/2        - imag radius)

	-- -- Curve
	-- let (cx':+cy') = (centre + size/2 - (radius:+radius)) in Cairo.arc cx' cy' radius 0 (π/2)

	-- -- Third line segment
	-- vectorise Cairo.moveTo (centre + size/2       - real radius)
	-- vectorise Cairo.lineTo (centre - flipy size/2 + real radius)

	-- -- Curve
	-- let (cx':+cy') = (centre - flipy size/2 + flipy (radius:+radius)) in Cairo.arc cx' cy' radius (π/2) π

	-- -- Fourth line segment
	-- vectorise Cairo.moveTo (centre - flipy size/2 - imag radius)
	-- vectorise Cairo.lineTo (centre - size/2       + imag radius)

	-- -- Curve
	-- let (cx':+cy') = (centre - size/2 + (radius:+radius)) in Cairo.arc cx' cy' radius π (3*π/2)

-- Images ----------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Factor out clip logic, document properly (cf. other anchored functions in this module)
-- TODO: Wrapper for images (?)
-- TODO: Factor out clip area (?)
anchoredImageWithClip :: (Complex Double -> Complex Double -> Cairo.Render ()) -> Complex Double -> Complex Double -> Cairo.Surface -> Cairo.Render ()
anchoredImageWithClip clip p anchor im = do
  size   <- imageSurfaceSize im
  centre <- return $ p - dotwise (*) anchor size
  clip centre size
  Cairo.clip
  vectorise Cairo.translate $ (centre - size*0.5)
  Cairo.setSourceSurface im 0 0
  -- vectorise Cairo.moveTo (centre - size*0.5)
  Cairo.paint
  vectorise Cairo.translate $ negate (centre - size*0.5)
  Cairo.resetClip


-- |
anchoredImage :: Complex Double -> Complex Double -> Cairo.Surface -> Cairo.Render ()
anchoredImage p anchor im = anchoredImageWithClip rectangle p anchor im


-- |
imageWithClip :: (Complex Double -> Complex Double -> Cairo.Render ()) -> Complex Double -> Cairo.Surface -> Cairo.Render ()
imageWithClip clip centre im = anchoredImageWithClip clip centre (0.0:+0.0) im


-- |
image :: Complex Double -> Cairo.Surface -> Cairo.Render ()
image p im = anchoredImageWithClip rectangle p (0.0:+0.0) im

-- Typography ------------------------------------------------------------------------------------------------------------------------------

-- | Renders the given string (with an arbitrary function 'draw'). The position of the centre of the bounding box is
--   given by the pin point 'p' and an 'anchor' (whose coordinates are normalised with respect to the text bounds).
-- TODO: Extract anchoring logic
-- TODO: Define anchor constants (eg. left:+top, centre:+bottom) (?)
anchoredText :: Complex Double -> Complex Double -> (String -> Cairo.Render a) -> String -> Cairo.Render a
anchoredText p anchor draw text = do
  extents <- textsize text
  vectorise Cairo.moveTo $ p - dotwise (*) (anchor + (0.5:+0.5)) extents
  draw text


-- |
centredText :: Complex Double -> (String -> Cairo.Render a) -> String -> Cairo.Render a
centredText centre draw text = anchoredText centre (0.0:+0.0) draw text
