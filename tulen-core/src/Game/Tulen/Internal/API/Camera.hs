module Game.Tulen.Internal.API.Camera(

  ) where

import Game.Tulen.API.Camera
import Game.Tulen.Internal.API.Helpers
import Game.Tulen.Internal.API.Math
import Game.Tulen.Internal.Camera
import Game.Tulen.Internal.ExternalRef
import Game.Tulen.Internal.Monad
import Graphics.Urho3D
import qualified Game.Tulen.API.Math as API

instance CameraMonad Spider TulenM where
  type Camera Spider TulenM = CameraId

  -- cameraRaycast :: Event t (Camera t m, ViewPoint) -> m (Event t WorldPos)
  cameraRaycast e = do
    core <- ask
    performEvent $ ffor e $ \(cid, API.V2 x y) -> do
      mres <- flip runReaderT core $ cameraRaycastSingle cid 2000 (IntVector2 x y)
      pure $ ffor mres $ \RayQueryResult{..} -> HitPoint {
          hitPointPosition = toAPI _rayQueryResultPosition
        , hitPointNormal = toAPI _rayQueryResultNormal
        , hitPointDistance = realToFrac _rayQueryResultDistance
        }
  {-# INLINE cameraRaycast #-}

  -- currentCamera :: m (Dynamic t (Camera t m))
  currentCamera = do
    camVar <- asks coreActiveCamera
    externalRefDynamic camVar
  {-# INLINE currentCamera #-}
