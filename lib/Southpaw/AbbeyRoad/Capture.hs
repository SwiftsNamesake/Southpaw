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
-- https://github.com/peterhil/hs-openal-proto/blob/master/src/Sound/OpenAL/Proto/Play.hs

-- TODO | - Proper error handling (IO exceptions, Either, or something else)
--        - 

-- SPEC | -
--        -



module Southpaw.AbbeyRoad.Capture where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Control.Concurrent (threadDelay)      --
import Control.Exception  (bracket, finally) --
import Control.Monad      (unless, when)     --

import Data.Int                --
import Data.List (intersperse) --
import Data.Word               --

import Foreign                   --
import Foreign.C.Types           --
import Foreign.ForeignPtr        --
import Foreign.ForeignPtr.Unsafe --
import Foreign.Ptr               --

import Sound.ALUT                --

import Sound.OpenAL                         --
import Sound.OpenAL.AL.BasicTypes (ALsizei) --
import Sound.OpenAL.ALC.Capture             --

import System.Exit (exitFailure)                    --
import System.IO (hPutStrLn, stderr)                --
import qualified Data.Vector.Storable as V          --
import qualified Data.Vector.Storable.Mutable as VM --

import GHC.Float (float2Double) --



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
type Sample = Double



---------------------------------------------------------------------------------------------------
-- Data
---------------------------------------------------------------------------------------------------
samplerate = 44100 :: Int -- TODO: Don't hard-code sample rate

mono8BufferSize    = bufferSize 1 (undefined :: Word8)
mono16BufferSize   = bufferSize 1 (undefined :: Int16)
stereo8BufferSize  = bufferSize 2 (undefined :: Word8)
stereo16BufferSize = bufferSize 2 (undefined :: Int16)



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
bufferSize :: Storable a => Int -> a -> Double -> Int
bufferSize nchannels sampleType secs = fromIntegral (numSamples secs) * sizeOf sampleType * nchannels


-- |
numSamples :: Double -> NumSamples
numSamples secs = round (fromIntegral(samplerate) * secs) :: NumSamples

--------------------------------------------------------------------------------

-- |
sine :: Double -> [Sample]
sine freq = cycle $ take n $ map sin [0, d..]
	 where d = 2 * pi * freq / sr
	       n = truncate (sr / freq)
	       sr = fromIntegral samplerate

--------------------------------------------------------------------------------

-- |
pcm :: (Integral a) => Int -> Sample -> a
pcm bits sample = truncate $ sample * (fromIntegral (2 ^ (bits - 1)) - 1)

--------------------------------------------------------------------------------

-- |
-- # From http://dev.stephendiehl.com/hask/#ffi
vecPtr :: VM.MVector s CInt -> ForeignPtr CInt
vecPtr = fst . VM.unsafeToForeignPtr0

--------------------------------------------------------------------------------

-- |
-- captureSamples :: Device -> Ptr a -> NumSamples -> IO ()
-- allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
-- allocaBytesAligned :: Int -> Int -> (Ptr a -> IO b) -> IO b
-- TODO: Find out why captureOpenDevice returns Nothing when secs is 0
-- TODO: Pass in failure action (?)
withCaptureDevice :: (Maybe String) -> Double -> (Device -> IO c) -> IO c -> IO c
withCaptureDevice specifier secs onsuccess onfailure = acquire >>= \ mdevice -> case mdevice of
		Just device -> bracket acquire finally onsuccess --
		Nothing     -> onfailure                          --
	where format      = Mono16
	      finally mic = captureStop mic >> captureCloseDevice mic
	      record mic  = captureStart mic >> return mic
	      acquire     = captureOpenDevice specifier (fromIntegral samplerate) format (numSamples secs)


-- |
-- TODO: Refactor
-- capture :: V.Storable a => (Maybe String) -> Double -> IO (MemoryRegion a)
-- capture :: Storable a => (Maybe String) -> Double -> (MemoryRegion a -> IO c) -> IO c
-- capture :: (Maybe String) -> Double -> IO (V.Vector Int16)
capture :: Maybe String -> Double -> (MemoryRegion CInt -> IO c) -> IO c -- According to GHCi
capture specifier duration action = do
	withCaptureDevice specifier duration $ \mic -> do
		sleep $ realToFrac duration                                                          -- Sleep until we should stop recording
		mutableV <- V.thaw . V.fromList . map (pcm 16) . take (fromIntegral num) $ sine 220  --
		withForeignPtr (vecPtr mutableV) $ \ptr -> captureSamples mic ptr (fromIntegral num) --
		rec <- V.freeze mutableV                                                             --
		let (mem, size) = V.unsafeToForeignPtr0 rec
		withForeignPtr mem $ \ptr -> action $ MemoryRegion ptr (fromIntegral size)
	where num   = (numSamples duration)
	      bytes = (mono16BufferSize duration)