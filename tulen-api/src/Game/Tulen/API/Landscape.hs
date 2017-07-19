module Game.Tulen.API.Landscape(
    LandscapeMonad(..)
  , TileId(..)
  , TileInfo(..)
  , TilePos
  , WorldPos
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

-- | Information about tileset
data TileInfo = TileInfo {
  tileInfoId   :: !TileId      -- ^ Unique id of tileset
, tileInfoName :: !String      -- ^ Human readable name
, tileInfoPath :: !ResourceRef -- ^ Path to texture atlas of tileset
} deriving (Eq, Ord, Show, Read, Generic)

-- | API for manipulation with landscape
class Monad m => LandscapeMonad t m where
  -- | Get name to tile id mapping for available tilesets
  tileset :: m (Dynamic t [TileInfo])

  -- | Set tile at given coordinates and notify when finished with old value of tile.
  tileSetter :: Event t (TilePos, TileId) -> m (Event t TileId)
