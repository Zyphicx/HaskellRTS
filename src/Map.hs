module Map where

-- Map --

data Map = Map 
         { tiles      :: [[Tile]] -- Rows -> Columns (Bottom -> Top, Left -> Right)
         , structures :: [(Position, Structure)] 
         }



-- Tile --

data Tile = FlatTile Int TileTexture                 -- Height, texture
          | RampTile Int Int TileTexture Orientation -- Height1, Height2, texture, orientation



data TileTexture = Grass 
                 | Sand


-- Structures --

data Structure = Core deriving (Show)


-- General types

type Position = (Int,Int)

data Orientation = North | West | East | South deriving (Show)

tileSize :: Int
tileSize = 40

coreSize :: Int
coreSize = 2*tileSize


-- Random test junk

basicTile = FlatTile 0 Grass

basicMap = Map (replicate 20 (replicate 40 basicTile)) [((0,0), Core), ((18*tileSize,38*tileSize), Core)]

showMultiList []     = ""
showMultiList (x:xs) = showList x ++ "\n" ++ showMultiList xs
  where
    showList []     = ""
    showList (x:xs) = show x ++ showList xs


instance Show Map where
  show (Map tiles structures) = (show structures) ++ "\n" ++ showMultiList tiles


instance Show Tile where
  show (FlatTile _ texture) = show texture
  show (RampTile _ _ texture _) = show texture


instance Show TileTexture where
  show Grass = "G"
  show Sand  = "S"