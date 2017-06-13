{-# LANGUAGE NoMonomorphismRestriction #-}
module Game.Tulen.Internal.Landscape.Types where

import Control.Lens (view)
import Data.Array.Repa (Array, U, DIM2, Z(..), (:.)(..))
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Vector.Unboxed (Unbox)
import Data.Word
import Linear

import qualified Data.Array.Repa as R
import qualified Data.Map.Strict as M

-- | Heightmap is int32 two dimensional array
type Heightmap = Array U DIM2 Float

-- | Cliffmap stores cliffs levels in 2*L format and
-- slopes as 2*L+1.
type Cliffmap = Array U DIM2 Word32

-- | Stores information whether there is water in given tile position.
type Watermap = Array U DIM2 Bool

-- | Stores information which tile ID is stored for given position
type Tilemap = Array U DIM2 Word8

-- | Opacity map for blending layer.
type BlendLayer = Array U DIM2 Word32

-- | Describes all landscape data
data Landscape = Landscape {
-- | Collection of chunks by it offset
  landscapeChunks        :: !(Map (V2 Int) LandChunk)
-- | Chunk size in tiles
, landscapeChunkSize     :: !Int
  -- | Global water level
, landscapeWaterLevel    :: !Float
  -- | Holds global info about tiles
, landscapeTiles         :: !(Vector TileInfo)
  -- | Holds global info about blending layers
, landscapeBlending      :: !(Vector BlendInfo)
  -- | Size of single tile in world units
, landscapeTileScale     :: !Float
  -- | Number of vertecies per tile side
, landscapeResolution    :: !Int
  -- | Size of the highest possible terrain point in world units
, landscapeVerticalScale :: !Float
}

-- | Get landscape positive direction bounds
landscapeHighBounds :: Landscape -> V2 Int
landscapeHighBounds Landscape{..}
  | M.null landscapeChunks = 0
  | otherwise = let
    coords = M.keys landscapeChunks
    maxx = maximum $ view _x <$> coords
    maxy = maximum $ view _y <$> coords
    in V2 maxx maxy

-- | Get landscape positive direction bounds
landscapeLowBounds :: Landscape -> V2 Int
landscapeLowBounds Landscape{..}
  | M.null landscapeChunks = 0
  | otherwise = let
    coords = M.keys landscapeChunks
    minx = minimum $ view _x <$> coords
    miny = minimum $ view _y <$> coords
    in V2 minx miny

-- | Chunk of landscape
data LandChunk = LandChunk {
  -- | Offset in landscape
  landChunkPos        :: !(V2 Int)
  -- | Heightmap that defines height of each point in landscape piece.
, landChunkHeightmap  :: !Heightmap
  -- | Info about cliffs
, landChunkCliffs     :: !Cliffmap
  -- | Info about water plains
, landChunkWater      :: !Watermap
  -- | Info where each tile type is placed
, landChunkTiles      :: !Tilemap
  -- | Info which variance of tile is used
, landChunkTilesVar   :: !Tilemap
  -- | Blend layers that forms basic texture of landscape
, landChunkBlending   :: !(Vector BlendLayer)
}

-- | Global metainfo for a tile in landscape
data TileInfo = TileInfo {
  -- | Reference to tile texture (TODO)
  tileResource :: !String
}

-- | Global metainfo for a blending layer
data BlendInfo = BlendInfo {
  -- | Refernce to blend texture (TODO)
  blendResource :: !String
}

-- | Get empty landscape with given size and default settings
emptyLandscape :: V2 Int -- ^ Size of landscape in chunks
  -> Landscape
emptyLandscape (V2 sx sy) = Landscape {
    landscapeChunks = M.fromList [(V2 x y, emptyLandChunk (V2 csize csize) (V2 x y) hres) |
        x <- [0 .. sx]
      , y <- [0 .. sy]
      ]
  , landscapeChunkSize = csize
  , landscapeWaterLevel = -1
  , landscapeTiles = mempty
  , landscapeBlending = mempty
  , landscapeTileScale = 1
  , landscapeResolution = 30
  , landscapeVerticalScale = 1000
  }
  where
    csize = 16
    hres = V2 1024 1024

-- | Get empty land chunk (simple plain with no tiles)
emptyLandChunk ::
     V2 Int -- ^ Size in tiles
  -- | Offset in landscape
  -> V2 Int
  -- | Resolution of heightmap
  -> V2 Int
  -> LandChunk
emptyLandChunk (V2 xs ys) pos (V2 hxs hys) = LandChunk {
    landChunkPos        = pos
  , landChunkHeightmap  = hmap
  , landChunkCliffs     = emptyArr 0
  , landChunkWater      = emptyArr False
  , landChunkTiles      = emptyArr 0
  , landChunkTilesVar   = emptyArr 0
  , landChunkBlending   = mempty
  }
  where
    hmap = R.computeS $ R.fromFunction (Z :. hys :. hxs) (const 0)
    emptyArr :: Unbox a => a -> Array U DIM2 a
    emptyArr v = R.computeS $ R.fromFunction (Z :. ys :. xs) (const v)
