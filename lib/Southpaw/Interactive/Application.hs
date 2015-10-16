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

-- TODO | - Refactor (move types to separate module, add lenses)
--        - Input queries, composite events (InputState type?)
--        - Redesign API (less fragile and add-hoc) (divide into app types, eg. full screen canvas, etc.)
--        - Optional event wrappers
--        - Printf-style event coupling (with a Listener class (?))

-- SPEC | -
--        -




--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC directives
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Southpaw.Interactive.Application where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex --
import Data.Functor --
import Data.IORef   --
import Data.Maybe   --

import Control.Lens hiding (set)
import Control.Monad       (liftM, when, void, forM) --
import Control.Applicative (pure, (<*>))

import Graphics.UI.Gtk                   --
import qualified Graphics.Rendering.Cairo as Cairo --

import Southpaw.Interactive.Types
import Southpaw.Interactive.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Rename (eg. no-listeners), create a separate default value (?)
nolisteners :: EventMap self s
nolisteners = EventMap Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


-- |
-- TODO: Rename (eg. no-listeners), create a separate default value (?)
-- defaultListeners :: EventMap s
-- defaultListeners = nolisteners { }



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- Master listeners ------------------------------------------------------------------------------------------------------------------------

-- TODO: Hoogle section

-- |
masterdraw :: (s -> Cairo.Render ()) -> IORef s -> Cairo.Render ()
masterdraw draw stateref = do
	state <- Cairo.liftIO $ readIORef stateref
	draw state

-- General utilities -----------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Make polymorphic
widgetSize :: WidgetClass self => self -> IO (Complex Int)
widgetSize widget = (:+) <$> widgetGetAllocatedWidth widget <*> widgetGetAllocatedHeight widget

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Rename (duh)
-- TODO: Refactor
-- TODO: Factor out default event handlers
-- TODO: Use state value from the App (?)
wholecanvas :: WidgetSettings Window state -> WidgetSettings DrawingArea state -> state -> IO (App FullCanvasLayout state)
wholecanvas windowSettings canvasSettings appstate = do
	initGUI

	window'  <- makeWidget windowNew      $ windowSettings
	frame'   <- frameNew
	canvas'  <- makeWidget drawingAreaNew $ canvasSettings
	stateref <- newIORef appstate

	containerAdd frame' canvas'
	set window' [ containerChild := frame' ]

	widgetShowAll window'
	return $ App { _window=window', _layout=FullCanvasLayout { _canvas=canvas' }, _appsize=_settingsize windowSettings, _state=stateref }


-- |
-- TODO: Use readstate for everything, or remove it (?)
-- TODO: Find out how to deal with 'onanimate'
-- TODO: Event wrappers (eg. draw)
attachListeners :: WidgetClass self => self -> IORef state -> EventMap self state -> IO ()
attachListeners self stateref eventmap = do
	simpleBind buttonPressEvent   onmousedown
	simpleBind buttonReleaseEvent onmouseup
	simpleBind motionNotifyEvent  onmousemotion
	simpleBind keyPressEvent      onkeydown
	simpleBind keyReleaseEvent    onkeyup
	simpleBind deleteEvent        ondelete
	-- simpleBind draw           (\emap -> maybe Nothing (void . Just . masterdraw dr) (ondraw emap))
	maybe pass (\ondr -> void $ (self `on` draw $ masterdraw ondr stateref)) (ondraw eventmap)
	maybe pass (\(fps, ona) -> void $ timeoutAdd (ona self stateref) 30) (onanimate eventmap)

	pass
	-- perhapsBind self key onanimate
	-- onresize, -- configureEvent
	-- ondrawIO
	-- perhapsBind self  onquit
	-- forM [(self `on` motionNotifyEvent,  onmousemotion),
	--       (self `on` buttonPressEvent,   onmousedown),
	-- 			(self `on` buttonReleaseEvent, onmouseup)] $ \(signal, prop) -> perhaps (prop eventmap) (\listener -> signal $ listener stateref)
	where
		simpleBind = maybeBind self eventmap stateref


-- |
pass :: Monad m => m ()
pass = return ()


-- |
-- bind :: WidgetClass self => self -> Signal ->
bind :: WidgetClass self => self -> Signal self callback -> IORef s -> (IORef s -> callback) -> IO (ConnectId self)
bind self event stateref action = self `on` event $ action stateref


-- |
maybeBind :: WidgetClass self => self -> EventMap self s -> IORef s -> Signal self callback -> (EventMap self s -> Maybe (IORef s -> callback)) -> IO ()
maybeBind self eventmap state event find = maybe pass (void . bind self event state) (find eventmap)


-- |
makeWidget :: WidgetClass self => IO self -> WidgetSettings self s -> IO self
makeWidget make settings = do
	widget <- make
	applySettings widget settings


-- |
applySettings :: WidgetClass self => self -> WidgetSettings self s -> IO self
applySettings self settings = do
	widgetSetSizeRequest self dx dy -- TODO: Use windowSetDefaultSize instead (?)
	widgetAddEvents self eventmasks
	return self
	where
		(dx:+dy)   = _settingsize settings
		eventmasks = _settingeventmasks settings

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Add setup argument (?)
-- TODO: Use 'IO (App s)' or 'App s' directly (?)
run :: IO (App layout state) -> IO ()
run makeApp = makeApp >> mainGUI
