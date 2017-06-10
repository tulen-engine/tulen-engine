{-# LANGUAGE NoMonomorphismRestriction #-}
module Game.Tulen.Internal.Landscape.Types where

import Data.Array.Repa (Array, U, DIM2, Z(..), (:.)(..))
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Word
import Linear

import qualified Data.Array.Repa as R

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
  landscapeChunks     :: !(Map (V2 Int) LandChunk)
-- | Chunk size in tiles
, landscapeChunkSize  :: !Int
  -- | Global water level
, landscapeWaterLevel :: !Double
  -- | Holds global info about tiles
, landscapeTiles      :: !(Vector TileInfo)
  -- | Holds global info about blending layers
, landscapeBlending   :: !(Vector BlendInfo)
}

-- | Chunk of landscape
data LandChunk = LandChunk {
  -- | Offset in landscape
  landChunkPos        :: !(V2 Int)
  -- | Heightmap that defines height of each point in landscape piece.
, landChunkHeightmap  :: !Heightmap
  -- | Number of vertecies per side of a tile of heightmap to generate.
, landChunkResolution :: !Int
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

-- | Get empty land chunk (simple plain with no tiles)
emptyLandChunk ::
     V2 Int -- ^ Size in tiles
  -- | Offset in landscape
  -> V2 Int
  -- | Number of vertecies per side of tile for heightmap to generate.
  -> Int
  -- | Resolution of heightmap
  -> V2 Int
  -> LandChunk
emptyLandChunk (V2 xs ys) pos res (V2 hxs hys) = LandChunk {
    landChunkPos        = pos
  , landChunkHeightmap  = hmap
  , landChunkResolution = res
  , landChunkCliffs     = emptyArr 0
  , landChunkWater      = emptyArr False
  , landChunkTiles      = emptyArr 0
  , landChunkTilesVar   = emptyArr 0
  , landChunkBlending   = mempty
  }
  where
    hmap = R.computeS $ R.fromFunction (Z :. hys :. hxs) (const 0)
    emptyArr v = R.computeS $ R.fromFunction (Z :. ys :. xs) (const v)
