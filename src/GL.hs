
-- | Initialize an OpenGL window using the GLFW-b library


module GL where

--------------------------------------------------------------------------------


import Data.Char hiding ( Space )

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef

import System.Exit
import System.IO.Unsafe as Unsafe

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL

--------------------------------------------------------------------------------


{-# NOINLINE theExitFlag #-}
theExitFlag :: IORef Bool
theExitFlag = Unsafe.unsafePerformIO $ newIORef False

{-# NOINLINE theRefreshTrigger #-}
theRefreshTrigger :: MVar ()
theRefreshTrigger = Unsafe.unsafePerformIO $ newMVar ()   -- newEmptyMVar


{-# NOINLINE theWindowSize #-}
theWindowSize :: IORef (Int,Int)
theWindowSize = Unsafe.unsafePerformIO $ newIORef $ error "window size not set"

{-# NOINLINE theDisplayFunction #-}
theDisplayFunction :: IORef (Window -> Double -> IO ())
theDisplayFunction = Unsafe.unsafePerformIO $ newIORef nodisplay where
  nodisplay _ _ = return ()

--------------------------------------------------------------------------------


triggerExit :: IO ()
triggerExit = writeIORef theExitFlag True

setWindowCoordSystem :: IO ()
setWindowCoordSystem = do
  (w,h) <- readIORef theWindowSize
  viewport $=! (Position 0 0 , Size (fromIntegral w) (fromIntegral h))
  matrixMode $=! Projection
  loadIdentity
  GL.ortho 0 (fromIntegral w) (fromIntegral h) 0 (-1) (1::Double)
  matrixMode $=! Modelview 0
  loadIdentity

--------------------------------------------------------------------------------


myErrorCallback :: GLFW.Error -> String -> IO ()
myErrorCallback err msg = do
  putStrLn $ msg ++ "(" ++ show err ++ ")"
  triggerExit

myKeyCallback :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
myKeyCallback _ key nrepeat keyState modif = 
  case key of
    Key'Escape -> triggerExit
    _          -> return ()

myCharCallback :: Window -> Char -> IO ()
myCharCallback _ char = do
  return ()

myWinCloseCallback :: Window -> IO ()
myWinCloseCallback window = triggerExit

myFrBufSizeCallback :: Window -> Int -> Int -> IO ()
myFrBufSizeCallback window xsiz ysiz = do
  -- writeIORef theWindowSize (xsiz,ysiz)

  -- putStrLn $ "framebuffer resized to " ++ show (xsiz,ysiz)

  return ()

myRefreshCallback :: Window -> IO ()
myRefreshCallback window = do
  -- putStrLn "refresh callback"

  _ <- tryTakeMVar theRefreshTrigger
  putMVar theRefreshTrigger ()
  return ()

--------------------------------------------------------------------------------


{-

frac :: Double -> Double 

frac x   = x - fromIntegral (floor x :: Int)



fmod :: Double -> Double -> Double

fmod x s = frac (x/s) * s

-}

--------------------------------------------------------------------------------


redraw :: Window -> IO ()
redraw window = do
  Just time <- getTime 

  (xsiz,ysiz) <- getFramebufferSize window   -- window resizing is broken in glfw

  writeIORef theWindowSize (xsiz,ysiz)       -- this helps a little.


  display <- readIORef theDisplayFunction
  display window time
  swapBuffers window

renderLoop :: Window -> IO ()
renderLoop window = loop  where
  loop = do
    _ <- takeMVar theRefreshTrigger
    redraw window
    exit <- readIORef theExitFlag
    unless exit loop

eventLoop :: IO ()
eventLoop = do
  threadDelay 1
  waitEvents
  exit <- readIORef theExitFlag
  unless exit eventLoop

--------------------------------------------------------------------------------
initGL :: IO precalc -> (precalc -> Window -> Double -> IO ()) -> IO ()
initGL userPrecalc userDisplay = do
  setErrorCallback (Just myErrorCallback)
  GLFW.init

  Just window <- createWindow 800 500 "window title" Nothing Nothing

  (xsiz,ysiz) <- getFramebufferSize window
  putStrLn $ "initial framebuffer size = " ++ show (xsiz,ysiz)
  writeIORef theWindowSize (xsiz,ysiz)

  major <- getWindowContextVersionMajor    window
  minor <- getWindowContextVersionMinor    window
  rev   <- getWindowContextVersionRevision window
  putStrLn $ "OpenGL context version = " ++ show major ++ "." ++ show minor ++ "." ++ show rev

  setWindowCloseCallback     window (Just myWinCloseCallback )
  setKeyCallback             window (Just myKeyCallback      )
  setCharCallback            window (Just myCharCallback     )
  setFramebufferSizeCallback window (Just myFrBufSizeCallback)
  setWindowRefreshCallback   window (Just myRefreshCallback  )  

  forkOS $ do 
    makeContextCurrent (Just window) 
    swapInterval 1                   
    precalc <- userPrecalc
    writeIORef theDisplayFunction (userDisplay precalc)
    renderLoop window

  eventLoop 

  putStrLn "calling terminate"
  terminate         
  exitWith ExitSuccess
--------------------------------------------------------------------------------

