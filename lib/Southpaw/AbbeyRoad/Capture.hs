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
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (unless, when)
import Data.Int
import Data.List (intersperse)
import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Sound.ALUT
import Sound.OpenAL
import Sound.OpenAL.AL.BasicTypes (ALsizei)
import Sound.OpenAL.ALC.Capture
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import GHC.Float (float2Double)

-- import Sound.OpenAL.Proto.Conversion
-- import Sound.OpenAL.Proto.Types (sampleRate)
-- import Sound.OpenAL.Proto.UnitGen (sine)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
type Sample = Double



---------------------------------------------------------------------------------------------------
-- Data
---------------------------------------------------------------------------------------------------
sampleRate = 44100 :: Int -- TODO: Don't hard-code sample rate

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
numSamples secs = round (fromIntegral(sampleRate) * secs) :: NumSamples

--------------------------------------------------------------------------------

-- |
sine :: Double -> [Sample]
sine freq = cycle $ take n $ map sin [0, d..]
	 where d = 2 * pi * freq / sr
	       n = truncate (sr / freq)
	       sr = fromIntegral sampleRate

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
withCaptureDevice :: (Maybe String) -> Double -> (Device -> IO c) -> IO c
withCaptureDevice specifier secs action = bracket acquire finally action
	where format      = Mono16
	      acquire     = do (Just mic) <- captureOpenDevice specifier (fromIntegral sampleRate) format (numSamples secs)
	                       captureStart mic
	                       return mic
	      finally mic = do captureStop mic
	                       captureCloseDevice mic


-- |
-- capture :: V.Storable a => (Maybe String) -> Double -> IO (MemoryRegion a)
-- capture :: Storable a => (Maybe String) -> Double -> (MemoryRegion a -> IO c) -> IO c
-- capture :: (Maybe String) -> Double -> IO (V.Vector Int16)
capture :: Maybe String -> Double -> (MemoryRegion CInt -> IO c) -> IO c -- According to GHCi
capture specifier duration action = do
	withCaptureDevice specifier duration $ \mic -> do
		-- TODO: Why do we sleep for three seconds before recording?
		-- Probably because we need to wait before invoking 'captureStop'
		-- TODO: Handle delay more flexibly (move audio to a different thread?)
		sleep $ realToFrac duration

		-- mutableV <- V.thaw $ V.replicate (fromIntegral num) (fromIntegral 0)
		mutableV <- V.thaw $ V.fromList $ map (pcm 16) $ take (fromIntegral num) $ sine 220
		withForeignPtr (vecPtr mutableV) $ \ptr -> do
			captureSamples mic ptr (fromIntegral num)
		rec <- V.freeze mutableV

		-- recordedBytes <- get $ captureNumSamples mic
		-- putStrLn $ "Got samples: " ++ (show recordedBytes)

		let (mem, size) = V.unsafeToForeignPtr0 rec
		withForeignPtr mem $ \ptr -> action $ MemoryRegion ptr (fromIntegral size)
	where num = (numSamples duration)
	      bytes = (mono16BufferSize duration)