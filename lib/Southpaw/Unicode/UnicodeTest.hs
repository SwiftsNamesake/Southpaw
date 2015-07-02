--
-- Name
-- UnicodeTest.hs
--
-- Description
-- Testing the Windows Unicode module found in this directory
-- 
-- Author
-- Jonatan H Sundqvist
--
-- Date
-- August 10 2014
--

-- NOTE
-- This solution may be redundant
-- (cf. utf8-string)

{-# LANGUAGE NoImplicitPrelude #-}

module Southpaw.Unicode.UnicodeTest where

import qualified Unicode.WinUnicodeConIO as Unicode

main = do
	Unicode.putStr "♔♕♖♗♘♙|♚♛♜♝♞♟"
	Unicode.getChar
