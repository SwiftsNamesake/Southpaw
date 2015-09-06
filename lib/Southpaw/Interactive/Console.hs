-- |
-- Module      : Southpaw.Interactive.Console
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 6 2015

-- TODO | - Markup
--        - Configuring console

-- SPEC | -
--        -



---------------------------------------------------------------------------------------------------
-- GHC Directives
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- API
---------------------------------------------------------------------------------------------------
module Southpaw.Interactive.Console where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Control.Monad (liftM)          --
import System.IO     (stdout, hFlush) --
import Text.Read     (readMaybe)      --
import Text.Printf                    --
-- import System.Console.ANSI        --



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- Console IO -------------------------------------------------------------------------------------
-- |
prompt :: Read b => String -> IO (Maybe b)
prompt message = do
	putStr message >> hFlush stdout
	line <- getLine
	return $ readMaybe line


-- |
-- TODO: Console cursor (?)
-- TODO: Rename (?)
-- TODO: Pass in 'represent' function rather than relying on Show instance (?)
-- TODO: Let 'options' be an IO action instead of a pure list (?)
-- TODO: Customised 'invalid choice' behaviour (etc.) (?)
chooseFromOptions :: Show a => String -> (a -> e) -> [a] -> IO (Either e a)
chooseFromOptions question bailout options = do
	possibly (return . Left . bailout) options $ \op -> do
		putStrLn question
		mapM option $ zip ([1..] :: [Int]) options
		choice <- untilM (valid options) (const $ prompt "That doesn't work. Try again: ") (prompt "Choose one: ")
		return $ case choice of
			Just index -> Right $ options !! (index-1) --
			Nothing    -> Left  $ "Invalid choice"   -- This should never happen, throw error instead (?)
	where
		valid options  = return . maybe False (clamped 0 (length options) . (subtract 1)) --
		prompt q       = putStr q >> hFlush stdout >> (liftM readMaybe) getLine           -- Ask for input (flush is sometimes required when q doesn't end in a newline)
		possibly f x g = either f g x                                                     -- Do-block at the end instead of in the middle
		option (n, op) = printf "  [%d] %s\n" n (show op)                                 -- Prints a model option


-- General utilities (should be moved eventually) -------------------------------------------------
-- | Like maybe, except the function comes last
-- TODO: Deport to Siber... I mean move to Utilities module
perhaps :: b -> Maybe a -> (a -> b) -> b
perhaps fallback value action = maybe fallback action value


-- | Monadic ternary operator
-- TOOD: Rename
assumingM :: Monad m => m Bool -> m a -> m a -> m a
assumingM p a b = p >>= \ done -> if done then a else b


-- | Perform an action until the returned value satisfies the condition
-- TODO: Pass in previous result (✓)
-- TODO: Refactor, rename variables (?)
-- TODO: Move to separate module (eg. loops/control structures) (?)
untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> m a -> m a
untilM p f x = do
	value <- x
	done  <- p value
	if done
	  then return value
	  else untilM p f (f value)


-- | a ∈ [low, upp]
clamped low upp a = (low <= a) && (a <= upp)
