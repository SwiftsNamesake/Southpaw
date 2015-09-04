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
import Data.Complex             --
import Data.IORef               --
import Data.Maybe

import Control.Monad (liftM, when)    --

import Graphics.UI.Gtk          --
import Graphics.Rendering.Cairo --



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Input state (?)
data App state = App { _window :: Window, _canvas :: DrawingArea, _size :: (Int, Int), _state :: IORef state } --
-- data AppState = AppState { _game :: Game, _selected :: Maybe Int, _path :: [Complex Double] }                    --


-- | High-level app setup (flags, config, cleanup, events, etc.)
-- TODO: Use more than one type variable (?)
-- TODO: Key maps, event maps
-- TODO: Rename type (?)
-- TODO: Assume stateref (?)
-- TODO: Wrapper for all key press events (?)
data EventMap s = EventMap {

	onmousedown :: Maybe (IORef s -> EventM EButton Bool),
	onmouseup   :: Maybe (IORef s -> EventM EButton Bool),
	-- onresize,
	-- ondrawIO

	onmousemotion :: Maybe (IORef s -> EventM EMotion Bool),

	onkeypress :: Maybe (IORef s -> EventM EKey Bool),

	onanimate :: Maybe (DrawingArea -> IORef s -> IO Bool),

	ondraw   :: Maybe (s -> Render ()), 

	ondelete :: Maybe (IORef s -> EventM EAny Bool)
	-- onquit :: IO s

}



---------------------------------------------------------------------------------------------------
-- Data
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Default event handling
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
-- TODO: Rename (duh)
-- TODO: Refactor
-- TODO: Factor out default event handlers
-- TODO: Use state value from the App (?)
createWindowWithCanvasAndEvents :: (Complex Int) -> Int -> s -> EventMap s -> IO (App s)
createWindowWithCanvasAndEvents (w:+h) fps appstate eventmap = do
	app <- createWindowWithCanvas w h appstate
	stateref <- return $ _state app
	perhaps (onanimate eventmap) (\animate -> timeoutAdd (animate (_canvas app) stateref) (1000 `div` fps))
	bindWindowEvents app eventmap
	return app
	where
	  perhaps :: Maybe a -> (a -> IO b) -> IO ()
	  perhaps ma f = maybe (return ()) (\a -> f a >> return ()) ma --


-- |
-- TODO: Use readstate for everything, or remove it (?)
bindWindowEvents :: App s -> EventMap s -> IO ()
bindWindowEvents app eventmap = do
	
	(window, canvas, stateref) <- return (_window app, _canvas app, _state app)

	perhaps (onmousemotion eventmap) $ \onmm -> window `on` motionNotifyEvent $ onmm stateref

	-- perhaps (onmouseclick eventmap) $ \onmc -> window `on` _ $ 
	perhapsBind window buttonPressEvent   onmousedown stateref
	perhapsBind window buttonReleaseEvent onmouseup   stateref
	-- perhaps (onmousedown eventmap) $ \omd -> window `on` buttonPressEvent   $ omd stateref
	perhaps (onmouseup   eventmap) $ \omu -> window `on` buttonReleaseEvent $ omu stateref

	-- perhaps (onresize     eventmap) $ \onrs -> window `on` _ $ 
    -- window `on` configureEvent $ onresize window worldref

	perhaps (onkeypress eventmap) $ \onkd -> window `on` keyPressEvent $ onkd stateref

	perhaps (ondraw eventmap) $ \ond -> canvas `on` draw $ (liftIO (readIORef stateref) >>= ond)
	-- let ondrawIO' = maybe readstate id (ondrawIO eventmap) $ stateref

	window `on` deleteEvent $ maybe (\_ -> liftIO $ mainQuit >> return False) (id) (ondelete eventmap) $ stateref

	return ()
	where
	  perhaps ma f = let pass = return () in maybe (pass) (\a -> f a >> pass) ma --
	  perhapsBind win event find state = perhaps (find eventmap) $ \action -> win `on` event $ action state

-- |
-- TODO: Factor out non-canvas logic for reuse
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
	print size

	-- 
	stateref <- newIORef appstate

	return $ App window canvas size stateref


-- |
-- TODO: Add setup argument (?)
-- TODO: Use 'IO (App s)' or 'App s' directly (?)
run :: IO (App s) -> IO ()
run makeApp = makeApp >> mainGUI