module Game.Tulen.Internal.Core.Types(
    Core(..)
  , CoreConfig(..)
  , defaultCoreConfig
  , UIElementId(..)
  ) where

import Control.Concurrent.STM
import Data.Bimap (Bimap)
import Data.Data
import Data.IORef
import Data.Map.Strict (Map)
import Foreign
import Game.Tulen.Internal.Camera.Types
import Game.Tulen.Internal.ExternalRef
import Game.Tulen.Internal.Landscape.Types
import GHC.Generics
import Graphics.Urho3D
import Reflex.Spider

newtype UIElementId = UIElementId { unUIElementId :: Int }
  deriving (Eq, Ord, Show, Read, Generic, Data)

-- | Main context of engine. Here goes all referencies to internal resources.
-- Core is used as entry point for game.
data Core = Core {
  coreApplication   :: SharedPtr Application
, coreScene         :: SharedPtr Scene
, coreCameras       :: TVar (Map CameraId CameraData, Int)
, coreActiveCamera  :: ExternalRef Spider CameraId
, coreConfig        :: CoreConfig
, coreUI            :: Ptr UI
, coreInput         :: Ptr Input
, coreResourceCache :: Ptr ResourceCache
, coreFileSystem    :: Ptr FileSystem
, coreOctree        :: Ptr Octree
, coreGraphics      :: Ptr Graphics
, coreCursor        :: Ptr Cursor
, coreRenderer      :: Ptr Renderer
, coreLandscape     :: ExternalRef Spider LoadedLandscape -- ^ TODO: replace with loaded map reference
, coreLandscapeChan :: TChan (Landscape -> Landscape, IO ()) -- ^ Queue of landscape changes, IO action notifies sender that land is patched
, coreUIElements    :: TVar (Bimap UIElementId (Ptr UIElement), Int)
}

-- | Additional runtime configuration of engine core.
data CoreConfig = CoreConfig {
  -- | Created window title
  coreWindowTitle :: String
  -- | Log file name relative to current folder. Nothing means no log.
, coreLogFile     :: Maybe String
  -- | Path to resource folder. For Nothing, try to guess.
, coreResources   :: Maybe FilePath
  -- | User can provide additional action for engine setup stage.
, coreCustomSetup :: Maybe (Core -> IO ())
  -- | User can provide additional action for engine startup stage.
, coreCustomStart :: Maybe (Core -> IO ())
-- | User can provide additional action for engine stop stage.
, coreCustomStop  :: Maybe (Core -> IO ())
}

-- | Neutral configuration that doesn't affect behavior of engine
defaultCoreConfig :: CoreConfig
defaultCoreConfig = CoreConfig {
    coreWindowTitle = "Tulen engine"
  , coreLogFile = Nothing
  , coreResources = Nothing
  , coreCustomSetup = Nothing
  , coreCustomStart = Nothing
  , coreCustomStop = Nothing
  }
