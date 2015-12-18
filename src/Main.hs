module Main (main) where

import Control.Concurrent
import Control.Monad
import Sound.OpenAL
import Foreign.Marshal.Array
import Data.Int

main :: IO ()
main = do
  Just dev <- captureOpenDevice Nothing 44100 MonoFloat32 2048
  spec <- get $ captureDeviceSpecifier dev
  print spec
  captureStart dev
  threadDelay 100000
  get (captureNumSamples dev) >>= print

  buf <- callocArray 2048
  do captureSamples dev buf 2048
     l2 <- peekArray 2048 buf :: IO [Float]
     print l2
  captureStop dev
  c <- captureCloseDevice dev
  print c
