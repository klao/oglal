{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import qualified Data.Vector.Unboxed as VU
import           Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW as GLFW

import           My.Sound

data Event
  = EvtWinClose
  | EvtResize !Int !Int
  deriving (Eq, Show)

data State
  = State { _stGoOn :: !Bool
          , _stW :: !Int
          , _stH :: !Int
          }
  deriving (Eq, Show)
makeLenses ''State

stWH :: Functor f => ((Int, Int) -> f (Int, Int)) -> State -> f State
stWH f s@(State {..}) = fmap (\(w,h) -> s { _stW = w, _stH = h }) $ f (_stW, _stH)

iniState :: State
iniState = State True 0 0

processEvents :: TQueue Event -> State -> IO State
processEvents evch = go
  where
    go st = do
      me <- atomically $ tryReadTQueue evch
      case me of
        Nothing -> return st
        Just ev -> do
          case ev of
            EvtWinClose -> go $ st & stGoOn .~ False
            EvtResize w h -> do
              viewport $= (Position (-fromIntegral w) 0,
                           Size (2 * fromIntegral w) (fromIntegral h))
              GL.loadIdentity
              scale (1 /  fromIntegral w) 0.5 (1.0 :: GLfloat)

              go $ st & stWH .~ (w,h)

drawingStuff :: GLFW.Window -> TQueue Event -> TQueue (GLfloat, GLfloat) -> IO ()
drawingStuff win evch spch = do
  GLFW.setWindowCloseCallback win $ Just $ \_ ->
    atomically $ writeTQueue evch EvtWinClose
  GLFW.setWindowSizeCallback win $ Just $ \_ x y ->
    atomically $ writeTQueue evch $ EvtResize x y

  -- This doesn't seem to work on Windows, that's why we need a
  -- threadDelay at the end of the loop
  -- GL.finish seems to be helping, but not completely :(
  GLFW.swapInterval 1
  depthFunc $= Just Always
  clearColor $= Color4 1 1 1 1
  loop (iniState & stW .~ 800 & stH .~ 600) []
  -- Force a resize event:
  atomically $ writeTQueue evch $ EvtResize 800 600

  where
  loop st l = do
    GLFW.pollEvents
    st' <- processEvents evch st
    l' <- take 10000 <$> consumeTQueue spch l

    clear [ColorBuffer]
    color $ Color4 0 0 0 (1 :: GLfloat)

    let w = fromIntegral $ st' ^. stW
    renderPrimitive Lines $ do
      forM_ (zip [w,w-1..(1::GLfloat)] l') $ \(x,(y0,y1)) -> do
        vertex $ Vertex2 x y0
        vertex $ Vertex2 x y1

    -- flush
    GLFW.swapBuffers win
    finish

    threadDelay 8300
    when (st ^. stGoOn) $ loop st' l'

consumeTQueue :: TQueue a -> [a] -> IO [a]
consumeTQueue q = go
  where
    go l = do
      mx <- atomically $ tryReadTQueue q
      case mx of
        Just x -> go (x:l)
        Nothing -> return l

soundProcessor :: TQueue (GLfloat, GLfloat) -> IO ()
soundProcessor spch = do
  bch <- newTQueueIO
  _ <- forkIO $ startCapture 128 (atomically . writeTQueue bch)
  forever $ do
    b <- atomically $ readTQueue bch
    atomically $ writeTQueue spch $ (VU.minimum b, VU.maximum b) & both %~ realToFrac

main :: IO ()
main = do
  GLFW.setErrorCallback $ Just $ \e s -> putStrLn $ show e ++ ": " ++ s
  True <- GLFW.init
  Just win <- GLFW.createWindow 800 600 "GLFW proba" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  evch <- newTQueueIO

  spch <- newTQueueIO
  _ <- forkIO $ soundProcessor spch

  drawingStuff win evch spch
  GLFW.destroyWindow win
  GLFW.terminate
