-- |
-- Module      : RenderUtils
-- Description : Cairo rendering utilities
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



module Southpaw.Picasso.RenderUtils where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import qualified Graphics.Rendering.Cairo as Cairo



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- General rendering utilities --------------------------------------------------------------------
-- |
-- TODO: General anchor (?)
renderCentredText :: Point -> String -> Cairo.Render ()
renderCentredText (cx:+cy) text = do
	extents <- Cairo.textExtents text
	let (w, h) = (Cairo.textExtentsWidth extents, Cairo.textExtentsHeight extents)
	Cairo.moveTo (cx-w/2) (cy+h/2)
	Cairo.showText text