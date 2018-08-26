module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Graphics.UI.GLFW as GLFW

type Position = (Int,Int)

data Map = Map 
         { tiles      :: [[Tile]] -- Rows -> Columns (Bottom -> Top, Left -> Right)
         , structures :: [(Position, Structure)] 
         }

data Orientation = North | West | East | South deriving (Show)

data Tile = FlatTile Int TileTexture      		     -- Height, texture
          | RampTile Int Int TileTexture Orientation -- Height1, Height2, texture, orientation

data Structure = Core deriving (Show)

data TileTexture = Grass 
             	 | Sand

instance Show Map where
  show (Map tiles structures) = (show structures) ++ "\n" ++ showMultiList tiles


instance Show Tile where
  show (FlatTile _ texture) = show texture
  show (RampTile _ _ texture _) = show texture

instance Show TileTexture where
  show Grass = "G"
  show Sand  = "S"

showMultiList []     = ""
showMultiList (x:xs) = showList x ++ "\n" ++ showMultiList xs
  where
  	showList [] 	= ""
  	showList (x:xs) = show x ++ showList xs


basicTile = FlatTile 0 Grass

basicMap = Map (replicate 20 (replicate 40 basicTile)) [((0,0), Core), ((18*tileSize,38*tileSize), Core)]

tileSize :: Int
tileSize = 40

coreSize :: Int
coreSize = 2*tileSize


main :: IO ()
main = do 
    withWindow 800 600 "RTS game test" $ \window -> do
      let loop = do 
            threadDelay 5000000
      loop
  

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