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



module Southpaw.Picasso.RenderUtils (renderCentredText) where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import qualified Graphics.Rendering.Cairo as Cairo
import Data.Complex



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