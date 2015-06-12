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

module Main where

import WinUnicodeConIO

main = do
	WinUnicodeConIO.putStr "♔♕♖♗♘♙|♚♛♜♝♞♟"
	getChar
