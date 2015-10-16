-- |
-- Module      : Southpaw.Interactive.Types
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
module Southpaw.Interactive.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.IORef
import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Input state (?)
-- TODO: Layout type (?)
data App layout state = App { _window :: Window, _layout :: layout, _appsize :: Complex Int, _state :: IORef state } --
-- data AppState = AppState { _game :: Game, _selected :: Maybe Int, _path :: [Complex Double] }                     --


-- |
-- TODO: Fill out
data WidgetSettings self state = WidgetSettings { _settingsize :: Complex Int, _settingeventmasks :: [EventMask], _settinglisteners :: EventMap state self }


-- |
data FullCanvasLayout = FullCanvasLayout { _canvas :: DrawingArea }


-- data WindowState = Layout { _window :: Window, _canvas :: DrawingArea, _size :: Complex Int }


-- | High-level app setup (flags, config, cleanup, events, etc.)
-- TODO: Use more than one type variable (?)
-- TODO: Lenses
-- TODO: Key maps, event maps
-- TODO: Rename type (?)
-- TODO: Assume stateref (?)
-- TODO: Wrapper for all key press events (?)
data EventMap self s = EventMap { onmousedown :: Maybe (IORef s -> EventM EButton Bool),
	                                onmouseup   :: Maybe (IORef s -> EventM EButton Bool),
														      -- onresize,
														      -- ondrawIO
														      -- onquit :: IO s
														      onmousemotion :: Maybe (IORef s -> EventM EMotion Bool),
														      onkeydown     :: Maybe (IORef s -> EventM EKey Bool),
														      onkeyup       :: Maybe (IORef s -> EventM EKey Bool),
														      onanimate     :: Maybe (Int,  self -> IORef s -> IO Bool), -- TODO: Figure out how to deal with FPS
														      ondraw        :: Maybe (s -> Cairo.Render ()),
														      ondelete      :: Maybe (IORef s -> EventM EAny Bool) }

-- Classes ---------------------------------------------------------------------------------------------------------------------------------
