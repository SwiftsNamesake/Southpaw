-- |
-- Module      : Southpaw.Math.Constants
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 1 2015

-- TODO | - Create re-export module (simply called 'Math') (?)
--        -

-- SPEC | -
--        -



---------------------------------------------------------------------------------------------------
-- API
---------------------------------------------------------------------------------------------------
module Southpaw.Math.Constants where



---------------------------------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------------------------------
-- | Ye old faithful constant.
π :: Floating f => f
π = pi


-- | Let's not start a flamewar.
τ :: Floating f => f
τ = 2*π
