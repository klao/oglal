module Main (main) where

import Control.Monad
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

drawingStuff :: GLFW.Window -> IO ()
drawingStuff win = do
  GLFW.swapInterval 1
  depthFunc $= Just Always
  clearColor $= Color4 1 1 1 1
  forever $ do
    clear [ColorBuffer]
    color $ Color4 0 0 0 (1 :: GLfloat)

    -- This works:
    multisample $= Enabled
    -- These don't work
    -- lineSmooth $= Enabled
    -- polygonSmooth $= Enabled
    -- hint LineSmooth $= Nicest
    -- hint PolygonSmooth $= Nicest
    renderPrimitive Polygon $ do
      vertex $ Vertex2 0 (0 :: GLfloat)
      vertex $ Vertex2 1 (0.1 :: GLfloat)
      vertex $ Vertex2 0 (0.67826 :: GLfloat)

    multisample $= Disabled
    -- lineSmooth $= Disabled
    -- polygonSmooth $= Disabled
    -- hint LineSmooth $= DontCare
    -- hint PolygonSmooth $= DontCare
    renderPrimitive Polygon $ do
      vertex $ Vertex2 0 (0 :: GLfloat)
      vertex $ Vertex2 (-1) (0.1 :: GLfloat)
      vertex $ Vertex2 0 (0.67826 :: GLfloat)
    -- swapBuffers
    GLFW.swapBuffers win


main :: IO ()
main = do
  GLFW.setErrorCallback $ Just $ \e s -> putStrLn $ show e ++ ": " ++ s
  True <- GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'Samples 8
  Just win <- GLFW.createWindow 800 600 "GLFW proba" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  drawingStuff win
  GLFW.destroyWindow win
  GLFW.terminate
