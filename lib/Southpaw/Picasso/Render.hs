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

-- SPEC | -
--        -



---------------------------------------------------------------------------------------------------
-- GHC directives
---------------------------------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}


---------------------------------------------------------------------------------------------------
-- API
---------------------------------------------------------------------------------------------------
module Southpaw.Picasso.Render where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.Complex
import Control.Monad (forM_, liftM, liftM2)

import Control.Monad.IO.Class

import qualified Graphics.Rendering.Cairo                         as Cairo
-- import qualified Graphics.Rendering.Cairo.Internal.Surfaces.Image as Image

import qualified Southpaw.Picasso.Palette     as Palette
import qualified Southpaw.Picasso.Shapes      as Shapes
import           Southpaw.Picasso.RenderUtils
import           Southpaw.Math.Constants
import           Southpaw.Cartesian.Plane.Utilities



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- General ----------------------------------------------------------------------------------------
-- |
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


-- |
-- TODO: Support asymmetrical crosshairs (?)
crosshairs :: Complex Double -> Complex Double -> Cairo.Render ()
crosshairs (cx:+cy) (dx:+dy) = do
    Cairo.moveTo (cx-dx/2) (cy)
    Cairo.lineTo (cx+dx/2) (cy)

    Cairo.moveTo (cx) (cy-dy/2)
    Cairo.lineTo (cx) (cy+dy/2)


-- |
arrow :: Complex Double -> Complex Double -> Double -> Double -> Double -> Cairo.Render ()
arrow from to sl sw hw = do
    let (first:rest) = closePath $ Shapes.arrow from to sl sw hw
    vectorise Cairo.moveTo first
    forM_ rest $ vectorise Cairo.lineTo


-- |
-- TODO: Extract argument conversion logic (centre/size vectors to unpacked left-top/dx/dy)
rectangle :: Complex Double -> Complex Double -> Cairo.Render ()
rectangle (cx:+cy) (dx:+dy) = Cairo.rectangle (cx-dx/2) (cy-dy/2) dx dy


-- |
-- TODO: Should anchor point be relative to centre or topleft corner, use normalised or absolute coords (?)
-- TODO: Less confusing terminology...
-- TODO: Refactor
anchoredRectangle :: Complex Double -> Complex Double -> Complex Double -> Cairo.Render ()
anchoredRectangle anchorpoint (dx:+dy) (ax:+ay) = rectangle (anchorpoint-(ax*dx:+ay*dy)+(0.5*dx:+0.5)) (dx:+dy)


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
    Cairo.fill


-- |
-- TODO: Options for colour, width, closed/open, etcairo.
path :: [Complex Double] -> Cairo.Render ()
path []      = return ()
path (p:ath) = do
    vectorise Cairo.moveTo p
    forM_ ath $ vectorise Cairo.lineTo
    Cairo.stroke


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


-- |
-- TODO: Wrapper for images (?)
image :: Complex Double -> Cairo.Surface -> Cairo.Render ()
image (cx:+cy) im = do
  (dx:+dy) <- imageSurfaceSize im

  Cairo.setSourceSurface im 0 0
  Cairo.rectangle (cx-dx/2) (cy-dy/2) dx dy
  Cairo.clip
  Cairo.paint
  Cairo.resetClip
