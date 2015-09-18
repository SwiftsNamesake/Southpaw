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
import Data.Complex
import Control.Monad.IO.Class (MonadIO)
import Control.Monad          (liftM, liftM2)

import qualified Graphics.Rendering.Cairo as Cairo

import qualified Southpaw.Picasso.Palette as Palette
import           Southpaw.Cartesian.Plane.Utilities



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- General rendering utilities --------------------------------------------------------------------



-- | Choose a colour
choose :: Palette.Colour Double -> Cairo.Render ()
choose (r, g, b, a) = Cairo.setSourceRGBA r g b a


-- Vector utilities -------------------------------------------------------------------------------
-- TODO: Move to another module (?)
-- |
vectorise :: (Double -> Double -> a) -> (Complex Double) -> a
vectorise f (x:+y) = f x y


---------------------------------------------------------------------------------------------------
-- |
imageSurfaceSize :: MonadIO m => Cairo.Surface -> m (Complex Double)
imageSurfaceSize im = liftM (dotmap fromIntegral) $ liftM2 (:+) (Cairo.imageSurfaceGetWidth im) (Cairo.imageSurfaceGetHeight im)

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
