module Game.Tulen.API.Camera(
    ViewPoint
  , CameraMonad(..)
  ) where

import Game.Tulen.API.Landscape
import Game.Tulen.API.Math
import Reflex

-- | Point in viewport. Origin is top-left corner and X axis goes right, Y axis
-- goes down. Units are pixels.
type ViewPoint = V2 Int

-- | API for manipulating player camera
class Monad m => CameraMonad t m where
  -- | Camera object reference
  type Camera t m :: *

  -- | Transform screen point to world position using the camera
  cameraRaycast :: Event t (Camera t m, ViewPoint) -> m (Event t WorldPos)

  -- | Return current active camera
  currentCamera :: m (Dynamic t (Camera t m))
