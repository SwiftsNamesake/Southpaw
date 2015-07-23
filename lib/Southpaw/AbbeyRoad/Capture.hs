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
-- I am forever indebted to the author of this module: https://github.com/peterhil/hs-openal-proto/blob/master/src/Sound/OpenAL/Proto/Play.hs
-- Saved me from having to figure out how to construct memory views by myself.

-- TODO | - Proper error handling (IO exceptions, Either, or something else)
--        - Remove redundant imports
--        - Include the legal note from Peter's repo

-- SPEC | -
--        -



module Southpaw.AbbeyRoad.Capture where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Control.Concurrent (threadDelay)  --
import Control.Exception  (bracket)      --
import Control.Monad      (unless, when) --

import Data.Int                --
import Data.List (intersperse) --
import Data.Word               --

import GHC.Float (float2Double) --

import Foreign                   -- Import the foreigners!
import Foreign.C.Types           -- 
import Foreign.ForeignPtr        -- 
import Foreign.ForeignPtr.Unsafe -- 
import Foreign.Ptr               -- 

import Sound.ALUT                -- 

import Sound.OpenAL                         -- 
import Sound.OpenAL.AL.BasicTypes (ALsizei) -- 
import Sound.OpenAL.ALC.Capture             -- 

import System.Exit (exitFailure)                                                                     -- 
import System.IO   (hPutStrLn, stderr, openBinaryFile, IOMode(ReadMode), hClose, hFileSize, hGetBuf) -- 

import qualified Data.Vector.Storable as V          --
import qualified Data.Vector.Storable.Mutable as VM --




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

---------------------------------------------------------------------------------------------------

-- |
sine :: Double -> [Sample]
sine freq = cycle $ take n $ map sin [0, d..]
	 where d = 2 * pi * freq / sr
	       n = truncate (sr / freq)
	       sr = fromIntegral samplerate

---------------------------------------------------------------------------------------------------

-- |
pcm :: (Integral a) => Int -> Sample -> a
pcm bits sample = truncate $ sample * (fromIntegral (2 ^ (bits - 1)) - 1)

---------------------------------------------------------------------------------------------------

-- |
-- # From http://dev.stephendiehl.com/hask/#ffi
vecPtr :: VM.MVector s CInt -> ForeignPtr CInt
vecPtr = fst . VM.unsafeToForeignPtr0

---------------------------------------------------------------------------------------------------

-- |
-- captureSamples :: Device -> Ptr a -> NumSamples -> IO ()
-- allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
-- allocaBytesAligned :: Int -> Int -> (Ptr a -> IO b) -> IO b
-- TODO: Find out why captureOpenDevice returns Nothing when secs is 0
-- TODO: Pass in failure action (?)
-- TODO: Rename (eg. 'record' to 'startRecording') (?)
withCaptureDevice :: (Maybe String) -> Double -> (Device -> IO c) -> IO (Maybe c)
withCaptureDevice specifier secs onsuccess = bracket acquire finally between
	where format       = Mono16
	      finally      = maybe (return False) $ \mic -> captureStop mic >> captureCloseDevice mic
	      record mic   = captureStart mic >> return mic
	      acquire      = captureOpenDevice specifier (fromIntegral samplerate) format (numSamples secs)
	      between mmic = case mmic of
	      	Just mic -> record mic >> onsuccess mic >>= return . Just
	      	Nothing  -> return Nothing


-- |
-- TODO: Refactor
-- capture :: Storable a => (Maybe String) -> Double -> (MemoryRegion a -> IO c) -> IO c
capture :: Maybe String -> Double -> (MemoryRegion CInt -> IO c) -> IO (Maybe c) -- According to GHCi
capture specifier duration action = withCaptureDevice specifier duration record
	where num        = numSamples duration
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