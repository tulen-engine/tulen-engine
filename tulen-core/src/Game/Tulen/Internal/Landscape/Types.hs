module Game.Tulen.Internal.Landscape.Types where

import Data.Array.Repa
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Word
import Linear

-- | Heightmap is int32 two dimensional array
type Heightmap = Array U DIM2 Word32

-- | Cliffmap stores cliffs levels in 2*L format and
-- slopes as 2*L+1.
type Cliffmap = Array U DIM2 Word32

-- | Stores information whether there is water in given tile position.
type Watermap = Array U DIM2 Bool

-- | Stores information which tile ID is stored for given position
type Tilemap = Array U DIM2 Word32

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
  -- | Number of vertecies per side of heightmap to generate.
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
