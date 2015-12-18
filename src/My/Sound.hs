{-# LANGUAGE ViewPatterns #-}

module My.Sound (
  startCapture,
  ) where

import Control.Concurrent
import Control.Monad
import Sound.OpenAL
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Int
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector)

frequency :: Num a => a
frequency = 44100

-- TODO(klao): consiter using Storable vector and converting the
-- buffer directly into the vector.

samplesToFloat :: Int16 -> Float
samplesToFloat x = fromIntegral x / 32768.0

captureLoop :: Int -> (Vector Float -> IO ()) -> Device -> Ptr Int16 -> IO ()
captureLoop samples callb dev buf = forever $ do
  let samplesi = fromIntegral samples
  avail <- get (captureNumSamples dev)
  if avail < samplesi
    then threadDelay $ (samples - fromIntegral avail) * 1000000 `div` frequency
    else do captureSamples dev buf samplesi
            l <- peekArray samples buf
            callb (VU.fromListN samples $ map samplesToFloat l)

startCapture
  :: Int                        -- ^ Number of samples to get at a time
  -> (Vector Float -> IO ())    -- ^ Callback to consume recorded samples
  -> IO ()
startCapture samples callb = do
  -- TODO(klao): is 2*samples enough? Detect overruns?
  Just dev <- captureOpenDevice Nothing 44100 Mono16 (fromIntegral $ 2 * samples)
  captureStart dev
  buf <- callocArray 2048

  captureLoop samples callb dev buf

  captureStop dev
  _ <- captureCloseDevice dev
  return ()