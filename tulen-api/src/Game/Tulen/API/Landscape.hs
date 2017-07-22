module Game.Tulen.API.Landscape(
    LandscapeMonad(..)
  , TileId(..)
  , TileInfo(..)
  , TilePos
  , WorldPos
  , FlatWorldPos
  , WorldRect
  , LandHeight
  ) where

import Game.Tulen.API.Math
import Game.Tulen.API.Resource
import GHC.Generics
import Reflex

-- | Unique tileset id
newtype TileId = TileId { unTileId :: Int }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Tile position in landscape. Starts at bottom left corner of map, grows
-- positive in top-right direction.
type TilePos = V2 Int

-- | Position in world. Starts at bottom left conrer of map, grows positive in
-- top-right direction.
--
-- Upward axis is Y. X and Z are plane axises.
type WorldPos = V3 Double

-- | Position in world without 'Y' axis. See 'WorldPos'.
type FlatWorldPos = V2 Double

-- | Rectangular region in world coordinates. See 'WorldPos'.
type WorldRect = Rect Double

-- | Landscape height in world units.
type LandHeight = Double

-- | Information about tileset
data TileInfo = TileInfo {
  tileInfoId   :: !TileId      -- ^ Unique id of tileset
, tileInfoName :: !String      -- ^ Human readable name
, tileInfoPath :: !ResourceRef -- ^ Path to texture atlas of tileset
} deriving (Eq, Ord, Show, Read, Generic)

-- | API for manipulation with landscape
class Monad m => LandscapeMonad t m | m -> t where
  -- | Get name to tile id mapping for available tilesets
  tileset :: m (Dynamic t [TileInfo])

  -- | Set tile at given coordinates and notify when finished with old value of tile.
  --
  -- Tile variation will be selected randomly.
  tileSetter :: Event t (TilePos, TileId) -> m (Event t TileId)

  -- | Set landscape height in given region and notify when finished.
  heightSetter :: Event t (WorldRect, FlatWorldPos -> LandHeight -> LandHeight) -> m (Event t ())
