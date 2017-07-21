module Game.Tulen.API.Camera(
    ViewPoint
  , HitPoint(..)
  , CameraMonad(..)
  ) where

import Game.Tulen.API.Landscape
import Game.Tulen.API.Math
import GHC.Generics
import Reflex

-- | Point in viewport. Origin is top-left corner and X axis goes right, Y axis
-- goes down. Units are pixels.
type ViewPoint = V2 Int

-- | Result of raycast
data HitPoint = HitPoint {
  -- | Where a ray hit geometry
  hitPointPosition :: !WorldPos
  -- | Normal vector of hit point
, hitPointNormal   :: !Normal
  -- | Distance traveled by ray until hit
, hitPointDistance :: !Double
  -- TODO: add reference to game objects if hit it
} deriving (Show, Generic)

-- | API for manipulating player camera
class Monad m => CameraMonad t m | m -> t where
  -- | Camera object reference
  type Camera t m :: *

  -- | Transform screen point to world position using the camera
  cameraRaycast :: Event t (Camera t m, ViewPoint) -> m (Event t (Maybe HitPoint))

  -- | Return current active camera
  currentCamera :: m (Dynamic t (Camera t m))
