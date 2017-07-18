module Game.Tulen.Internal.Camera(
    cursorRaycastSingle
  ) where

import Control.Monad.Catch
import Data.Maybe
import Foreign
import Game.Tulen.Internal.Utils
import Graphics.Urho3D
import qualified Data.Vector as V

-- | Raycast from cursor pos with given camera and return first object hit
cursorRaycastSingle :: (MonadIO m, MonadMask m)
  => SharedPtr Application -- ^ Application reference
  -> Ptr Camera -- ^ Camera to cast from
  -> Float -- ^ Maximum hit distance
  -> m (Maybe RayQueryResult)
cursorRaycastSingle app camera maxDistance = do
  ui :: Ptr UI <- guardJust "UI" =<< getSubsystem app
  -- Check the cursor is visible and there is no UI element in front of the cursor
  mcursor <- wrapNullPtr <$> uiCursor ui
  case mcursor of
    Nothing -> pure Nothing
    Just cursor -> do
      isVisible <- uiElementIsVisible cursor
      pos@(IntVector2 px py) <- uiGetCursorPosition ui
      mres <- uiGetElementAt ui pos True
      if not isVisible || isJust mres then pure Nothing
        else do
          graphics :: Ptr Graphics <- guardJust "Graphics" =<< getSubsystem app
          width <- graphicsGetWidth graphics
          height <- graphicsGetHeight graphics
          cameraRay <- cameraGetScreenRay camera (fromIntegral px / fromIntegral width) (fromIntegral py / fromIntegral height)
          -- Pick only geometry objects, not eg. zones or lights, only get the first (closest) hit
          withObject (cameraRay, RayTriangle, maxDistance, drawableGeometry, defaultViewMask) $ \(query :: Ptr RayOctreeQuery) -> do
            scene <- componentGetScene camera
            octree :: Ptr Octree <- guardJust "Octree" =<< nodeGetComponent scene True
            octreeRaycastSingle octree query
            results <- rayOctreeQueryGetResult query
            pure $ if V.null results then Nothing
              else Just $ V.head results
