module Game.Tulen.Internal.Camera.Types(
    CameraId(..)
  , CameraData(..)
  ) where

import Foreign
import GHC.Generics
import Graphics.Urho3D

-- | Camera unique index
newtype CameraId = CameraId { unCameraId :: Int }
  deriving (Eq, Ord, Show, Generic)

-- | Runtime camera data
data CameraData = CameraData {
  cameraId            :: !CameraId
, cameraYaw           :: !Float
, cameraPitch         :: !Float
, cameraDebugGeometry :: !Bool
, cameraNode          :: !(Ptr Node)
, cameraPtr           :: !(Ptr Camera)
}
