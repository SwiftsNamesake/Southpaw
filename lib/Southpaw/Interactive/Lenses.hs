-- |
-- Module      : Southpaw.Interactive.Lenses
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 15 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------




--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Southpaw.Interactive.Lenses where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.IORef
import Data.Complex
import Data.Functor ((<$>))
import Control.Lens

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import qualified Southpaw.Interactive.Types as App



--------------------------------------------------------------------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------------------------------------------------------------------

-- App -------------------------------------------------------------------------------------------------------------------------------------

window :: Lens (App.App layout state) (App.App layout state) Window Window
window f s = (\new -> s { App._window=new }) <$> f (App._window s)


layout :: Lens (App.App layout state) (App.App layout state) layout layout
layout f s = (\new -> s { App._layout=new }) <$> f (App._layout s)


-- TODO: Make this a getter (since setting this property won't affect the windw size) (?)
size :: Lens (App.App layout state) (App.App layout state) (Complex Int) (Complex Int)
size f s = (\new -> s { App._appsize=new }) <$> f (App._appsize s)


state :: Lens (App.App layout state) (App.App layout state) (IORef state) (IORef state)
state f s = (\new -> s { App._state=new }) <$> f (App._state s)

-- CanvasLayout ----------------------------------------------------------------------------------------------------------------------------

canvas :: Lens (App.FullCanvasLayout) (App.FullCanvasLayout) (DrawingArea) (DrawingArea)
canvas f s = (\new -> s { App._canvas=new }) <$> f (App._canvas s)

-- Settings --------------------------------------------------------------------------------------------------------------------------------

-- Generic ---------------------------------------------------------------------------------------------------------------------------------
