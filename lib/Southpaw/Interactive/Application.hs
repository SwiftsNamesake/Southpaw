-- |
-- Module      : Application
-- Description : Utilties for creating GUI applications
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



module Southpaw.Interactive.Application where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Graphics.UI.Gtk          --
import Graphics.Rendering.Cairo --
import Data.IORef               --



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Input state (?)
data App state = App { _window :: Window, _canvas :: DrawingArea, _size :: (Int, Int), _state :: IORef state } --
-- data AppState = AppState { _game :: Game, _selected :: Maybe Int, _path :: [Complex Double] }                    --



---------------------------------------------------------------------------------------------------
-- Data
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Make polymorphic
widgetSize :: WidgetClass self => self -> IO (Int, Int)
widgetSize widget = do
	w <- widgetGetAllocatedWidth widget
	h <- widgetGetAllocatedHeight widget
	return (w, h)


-- |
createWindowWithCanvas :: Int -> Int -> appstate -> IO (App appstate)
createWindowWithCanvas w h appstate = do
	--
	initGUI
	window <- windowNew

	frame  <- frameNew
	canvas <- drawingAreaNew
	containerAdd frame canvas

	set window [ containerChild := frame ]
	windowSetDefaultSize window w h

	widgetAddEvents canvas [PointerMotionMask] -- MouseButton1Mask

	widgetShowAll window

	-- Animation
	size <- widgetSize window

	-- 
	stateref <- newIORef appstate

	return $ App window canvas size stateref
