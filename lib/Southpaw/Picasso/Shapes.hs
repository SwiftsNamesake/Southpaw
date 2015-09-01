-- |
-- Module      : Southpaw.Picasso.Shapes
-- Description : Various functions for creating mathematical shapes
-- Copyright   : (c) Jonatan H Sundqvist, year
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created date year

-- TODO | - 
--        - 

-- SPEC | -
--        -



---------------------------------------------------------------------------------------------------
-- API
---------------------------------------------------------------------------------------------------
module Southpaw.Picasso.Shapes where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.Complex

import Southpaw.Math.Constants



---------------------------------------------------------------------------------------------------
-- Function
---------------------------------------------------------------------------------------------------
-- Geometry ---------------------------------------------------------------------------------------
-- |
-- TODO: Start angle
-- TODO: Invalid arguments (eg. sides < 3) (use Maybe?)
-- TODO: Make polymorphic
-- TODO: Move to geometry section
-- polygon :: (Floating f, RealFloat f) => Int -> f -> Complex f -> [Complex f]
polygon :: Integral int => int -> Double -> Complex Double -> [Complex Double]
polygon sides radius origin = [ let θ = arg n in origin + ((radius * cos θ):+(radius * sin θ)) | n <- [1..sides]]
    where arg n = fromIntegral n * (2*π)/fromIntegral sides



-- |
-- TODO: Simplify and expound comments
arrow :: Complex Double -> Complex Double -> Double -> Double -> Double -> [Complex Double]
arrow from to sl sw hw = [from     + straight (sw/2), --
                          shaftEnd + straight (sw/2), --
                          shaftEnd + straight (hw/2), --
                          to,                         --
                          shaftEnd - straight (hw/2), --
                          shaftEnd - straight (sw/2), --
                          from     - straight (sw/2)] --
    where along a b distance = (a +) . mkPolar distance . snd . polar $ b-a -- Walk distance along the from a to b
          normal a b = let (mag, arg) = polar (b-a) in mkPolar mag (arg+π/2)
          shaftEnd = along from to sl --
          straight = along (0:+0) (normal from to) -- Vector perpendicular to the centre line

