-- |
-- Module      : Southpaw.AbbeyRoad.Capture
-- Description : Microphone input
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created July 22 2015
-- 
-- I am forever indebted to the author of this module: https://github.com/peterhil/hs-openal-proto/blob/master/src/Sound/OpenAL/Proto/Play.hs,
-- who saved me from having to figure out how to construct memory views by myself.

-- TODO | - Proper error handling (IO exceptions, Either, or something else)
--        - Remove redundant imports
--        - Include the legal note from Peter's repo

-- SPEC | -
--        -



module Southpaw.AbbeyRoad.Capture where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Control.Exception  (bracket)      --

import Foreign                   -- Import the foreigners!
import Foreign.C.Types           -- 

import Sound.ALUT                -- 

import Sound.OpenAL                         -- 
import Sound.OpenAL.AL.BasicTypes ()        -- 
import Sound.OpenAL.ALC.Capture             -- 

import qualified Data.Vector.Storable as V          --
import qualified Data.Vector.Storable.Mutable as VM --




---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
type Sample = Double



---------------------------------------------------------------------------------------------------
-- Data
---------------------------------------------------------------------------------------------------
samplerate :: Int
samplerate = 44100 -- TODO: Don't hard-code sample rate



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------

-- |
bufferSize :: Storable a => Int -> a -> Double -> Int
bufferSize nchannels sampleType secs = fromIntegral (numSamples secs) * sizeOf sampleType * nchannels


-- |
numSamples :: Double -> NumSamples
numSamples secs = round (fromIntegral samplerate * secs)

---------------------------------------------------------------------------------------------------

-- |
sine :: Double -> [Sample]
sine freq = cycle $ take n $ map sin [0, d..]
	 where
	   d  = 2 * pi * freq / sr
	   n  = truncate (sr / freq)
	   sr = fromIntegral samplerate

---------------------------------------------------------------------------------------------------

-- |
pcm :: (Integral a) => Int -> Sample -> a
pcm bits sample = truncate $ sample * (fromIntegral $ ((2 :: Int) ^ (bits - 1)) - 1)

---------------------------------------------------------------------------------------------------

-- |
-- # From http://dev.stephendiehl.com/hask/#ffi
vecPtr :: VM.MVector s CInt -> ForeignPtr CInt
vecPtr = fst . VM.unsafeToForeignPtr0

---------------------------------------------------------------------------------------------------

-- |
-- TODO: Find out why captureOpenDevice returns Nothing when secs is 0
-- TODO: Pass in failure action (?)
-- TODO: Rename (eg. 'record' to 'startRecording') (?)
withCaptureDevice :: (Maybe String) -> Double -> (Device -> IO c) -> IO (Maybe c)
withCaptureDevice specifier secs onsuccess = bracket acquire finally between
	where
	  format       = Mono16
	  finally      = maybe (return False) $ \mic -> captureStop mic >> captureCloseDevice mic
	  record mic   = captureStart mic >> return mic
	  acquire      = captureOpenDevice specifier (fromIntegral samplerate) format (numSamples secs)
	  between mmic = case mmic of
	    Just mic -> record mic >> onsuccess mic >>= return . Just
	    Nothing  -> return Nothing


-- |
-- TODO: Refactor
capture :: Maybe String -> Double -> (MemoryRegion CInt -> IO c) -> IO (Maybe c) -- According to GHCi
capture specifier duration action = withCaptureDevice specifier duration record
	where 
	  num        = numSamples duration
	  record mic = do
	    sleep $ realToFrac duration                                                          -- Sleep until we should stop recording
	    mutableV <- V.thaw . V.fromList . map (pcm 16) . take (fromIntegral num) $ sine 220  -- 
	    withForeignPtr (vecPtr mutableV) $ \ptr -> captureSamples mic ptr (fromIntegral num) -- 
	    rec <- V.freeze mutableV                                                             -- 
	    let (mem, size) = V.unsafeToForeignPtr0 rec                                          -- 
	    withForeignPtr mem $ \ptr -> action $ MemoryRegion ptr (fromIntegral size)           -- 

---------------------------------------------------------------------------------------------------

-- |
checkAlErrors :: IO [String]
checkAlErrors = do
	errs <- get $ alErrors
	return [ d | ALError _ d <- errs ]


-- |
checkAlcErrors :: Device -> IO [String]
checkAlcErrors device = do
	errs <- get $ alcErrors device
	return [ d | ALCError _ d <- errs ]