module Backend (withWindow, exitPressed) where

import Control.Monad (when)
import Graphics.UI.GLFW as GLFW

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO () 
withWindow width height title f = do 
      GLFW.setErrorCallback $ Just simpleErrorCallback 
      r <- GLFW.init 
      when r $ do 
        m <- GLFW.createWindow width height title Nothing Nothing 
        case m of 
          (Just win) -> do 
            GLFW.makeContextCurrent m 
            f win 
            GLFW.setErrorCallback $ Just simpleErrorCallback 
            GLFW.destroyWindow win 
          Nothing -> return () 
      GLFW.terminate 
  where 
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

exitPressed :: Window -> IO Bool
exitPressed window = fmap isPressed $ getKey window Key'Escape

isPressed :: KeyState -> Bool
isPressed keyState = keyState == KeyState'Pressed