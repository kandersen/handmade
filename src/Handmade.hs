module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import System.Exit (exitFailure)

glVersionString :: GLFW.Window -> IO String
glVersionString window = do
  maj <- show <$> GLFW.getWindowContextVersionMajor window
  min <- show <$> GLFW.getWindowContextVersionMinor window
  return $ maj ++ "." ++ min

printErrorCallback :: GLFW.ErrorCallback
printErrorCallback err string = do
  putStrLn . show $ err
  putStrLn string

keyCallback :: GLFW.KeyCallback
keyCallback window GLFW.Key'Escape _ GLFW.KeyState'Pressed _ =
  GLFW.setWindowShouldClose window True
keyCallback window key int keyState modifiers = do
  putStr $ show key
  putStr " "
  putStr $ show keyState
  putStr " "
  putStrLn $ show int
  
initGLFWOrExit :: IO ()
initGLFWOrExit = do
  putStrLn "Initializing GLFW..."
  GLFW.setErrorCallback (Just printErrorCallback)
  initWasSuccessful <- GLFW.init
  when (not initWasSuccessful) exitFailure
  putStr "Initialization successful, GLFW Version "
  maybe (return ()) putStrLn =<< GLFW.getVersionString

createWindowOrExit :: IO GLFW.Window
createWindowOrExit = do
  putStrLn "Creating window..."
  mWindow <- GLFW.createWindow 640 480 "Handmade Haskell" Nothing Nothing
  case mWindow of
    Nothing -> GLFW.terminate >> exitFailure
    Just window -> do
      putStr "Window creation successful, OpenGL Version "
      GLFW.makeContextCurrent (Just window)
      putStrLn =<< glVersionString window
      return window

shutDown :: GLFW.Window -> IO ()
shutDown window = do
  putStrLn "Shutting down Haskell Hero"
  GLFW.destroyWindow window
  GLFW.terminate
  putStrLn "Shut down successful. Thanks for playing!"
  
mainLoop :: GLFW.Window -> IO ()
mainLoop window = do
  GLFW.pollEvents
  shouldClose <- GLFW.windowShouldClose window
  (width, height) <- GLFW.getFramebufferSize window
  let ratio = fromIntegral width / fromIntegral height
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho (-ratio) ratio (-1) 1 1 (-1)
  GL.matrixMode $= GL.Modelview 0

  GL.loadIdentity
  t <- maybe 0 realToFrac <$> GLFW.getTime :: IO GL.GLfloat
  GL.rotate (t * 50.0) (GL.Vector3 0.0 0.0 1.0)
  GL.renderPrimitive GL.Triangles $ do
    GL.color (GL.Color3 1 0 0 :: GL.Color3 GL.GLfloat)
    GL.vertex (GL.Vertex3 (-0.6) (-0.4) 0 :: GL.Vertex3 GL.GLfloat)
    GL.color (GL.Color3 0 1 0 :: GL.Color3 GL.GLfloat)
    GL.vertex (GL.Vertex3 0.6 (-0.4) 0 :: GL.Vertex3 GL.GLfloat)
    GL.color (GL.Color3 0 0 1 :: GL.Color3 GL.GLfloat)
    GL.vertex (GL.Vertex3 0 0.6 0 :: GL.Vertex3 GL.GLfloat)

  GLFW.swapBuffers window
  when (not shouldClose) $ mainLoop window

main :: IO ()
main = do
  initGLFWOrExit
  GLFW.defaultWindowHints
  window <- createWindowOrExit
  GLFW.setKeyCallback window (Just keyCallback)
  mainLoop window
  shutDown window
