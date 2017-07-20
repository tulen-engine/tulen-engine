module Game.Tulen.Internal.Camera(
    CameraId(..)
  , CameraData(..)
  , newCamera
  , withCamera
  , updateCamera
  , moveCamera
  , cameraRaycastSingle
  , cursorRaycastSingle
  ) where

import Control.Concurrent.STM
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.STM
import Data.Bifunctor
import Data.IORef
import Data.Maybe
import Foreign hiding (void)
import Game.Tulen.Internal.Utils
import GHC.Generics
import Graphics.Urho3D

import Game.Tulen.Internal.Camera.Types
import Game.Tulen.Internal.Core.Types

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

-- | Create new camera and register it in core (thread safe)
newCamera :: (MonadIO m, MonadReader Core m) => Ptr Node -> m (Maybe CameraData)
newCamera node = do
  mcam <- nodeCreateComponent node Nothing Nothing
  case mcam of
    Nothing -> pure Nothing
    Just cam -> do
      camerasTVar <- asks coreCameras
      liftIO . atomically $ do
        (cameras, n) <- readTVar camerasTVar
        let camData = CameraData {
              cameraId = CameraId n
            , cameraYaw = 0
            , cameraPitch = 0
            , cameraDebugGeometry = False
            , cameraNode = node
            , cameraPtr = cam
            }
        writeTVar camerasTVar (M.insert (CameraId n) camData cameras, n+1)
        pure $ Just camData

-- | Update camera with given id
updateCamera :: (MonadIO m, MonadReader Core m) => CameraId -> (CameraData -> m CameraData) -> m ()
updateCamera i f = void $ withCamera i $ \d -> do
  var <- asks coreCameras
  d' <- f d
  liftIO . atomically $ modifyTVar' var $ first $ M.insert i d'

-- | Read input and moves the camera.
moveCamera :: (MonadIO m, MonadReader Core m) => Float -> CameraData -> m CameraData
moveCamera t camData = do
  ui <- asks coreUI
  input <- asks coreInput
  cursor <- asks coreCursor
  -- Right mouse button controls mouse cursor visibility: hide when pressed
  isRightPress <- inputGetMouseButtonDown input mouseButtonRight
  uiElementSetVisible cursor $ not isRightPress

  -- Do not move if the UI has a focused element (the console)
  mFocusElem <- uiFocusElement ui
  whenNothing mFocusElem camData $ do
    -- Movement speed as world units per second
    let moveSpeed = 20
    -- Mouse sensitivity as degrees per pixel
    let mouseSensitivity = 0.1
    let node = cameraNode camData

    -- Use this frame's mouse motion to adjust camera node yaw and pitch. Clamp the pitch between -90 and 90 degrees
    isVisible <- uiElementIsVisible cursor
    (yaw, pitch) <- if not isVisible then do
        mouseMove <- inputGetMouseMove input
        let yaw = cameraYaw camData + mouseSensitivity * fromIntegral (mouseMove ^. x)
        let pitch = clamp (-90) 90 $ cameraPitch camData + mouseSensitivity * fromIntegral (mouseMove ^. y)

        -- Construct new orientation for the camera scene node from yaw and pitch. Roll is fixed to zero
        nodeSetRotation node $ quaternionFromEuler pitch yaw 0
        pure (yaw, pitch)
      else pure (cameraYaw camData, cameraPitch camData)

    -- Construct new orientation for the camera scene node from yaw and pitch. Roll is fixed to zero
    nodeSetRotation node $ quaternionFromEuler pitch yaw 0

    -- Read WASD keys and move the camera scene node to the corresponding direction if they are pressed
    -- Use the Translate() function (default local space) to move relative to the node's orientation.
    whenM (inputGetKeyDown input KeyW) $
      nodeTranslate node (vec3Forward `mul` (moveSpeed * t)) TS'Local
    whenM (inputGetKeyDown input KeyS) $
      nodeTranslate node (vec3Back `mul` (moveSpeed * t)) TS'Local
    whenM (inputGetKeyDown input KeyA) $
      nodeTranslate node (vec3Left `mul` (moveSpeed * t)) TS'Local
    whenM (inputGetKeyDown input KeyD) $
      nodeTranslate node (vec3Right `mul` (moveSpeed * t)) TS'Local

    -- Toggle debug geometry with space
    spacePressed <- inputGetKeyPress input KeySpace

    return camData {
        cameraYaw = yaw
      , cameraPitch = pitch
      , cameraDebugGeometry = (if spacePressed then not else id) $ cameraDebugGeometry camData
      }
  where
    mul (Vector3 a b c) v = Vector3 (a*v) (b*v) (c*v)

-- | Find camera by id and perform action with it
withCamera :: (MonadReader Core m, MonadIO m) => CameraId -> (CameraData -> m a) -> m (Maybe a)
withCamera i f = do
  camerasRef <- asks coreCameras
  (cameras, _) <- liftIO . atomically . readTVar $ camerasRef
  maybe (pure Nothing) (fmap Just . f) $ M.lookup i cameras

-- | Raycast from given pos with given camera and return first object hit
cameraRaycastSingle :: (MonadIO m, MonadMask m, MonadReader Core m)
  => CameraId -- ^ Camera to cast from
  -> Float -- ^ Maximum hit distance
  -> IntVector2 -- ^ Position in screen
  -> m (Maybe RayQueryResult)
cameraRaycastSingle cid maxDistance (IntVector2 px py) = fmap join . withCamera cid $ \CameraData{..} -> do
  graphics <- asks coreGraphics
  octree <- asks coreOctree
  width <- graphicsGetWidth graphics
  height <- graphicsGetHeight graphics
  cameraRay <- cameraGetScreenRay cameraPtr (fromIntegral px / fromIntegral width) (fromIntegral py / fromIntegral height)
  -- Pick only geometry objects, not eg. zones or lights, only get the first (closest) hit
  withObject (cameraRay, RayTriangle, maxDistance, drawableGeometry, defaultViewMask) $ \(query :: Ptr RayOctreeQuery) -> do
    octreeRaycastSingle octree query
    results <- rayOctreeQueryGetResult query
    pure $ if V.null results then Nothing
      else Just $ V.head results

-- | Raycast from cursor pos with given camera and return first object hit
cursorRaycastSingle :: (MonadIO m, MonadMask m, MonadReader Core m)
  => CameraId -- ^ Camera to cast from
  -> Float -- ^ Maximum hit distance
  -> m (Maybe RayQueryResult)
cursorRaycastSingle cid maxDistance = fmap join . withCamera cid $ \CameraData{..} -> do
  ui <- asks coreUI
  cursor <- asks coreCursor
  graphics <- asks coreGraphics
  octree <- asks coreOctree
  isVisible <- uiElementIsVisible cursor
  pos@(IntVector2 px py) <- uiGetCursorPosition ui
  mres <- uiGetElementAt ui pos True
  if not isVisible || isJust mres then pure Nothing
    else do
      width <- graphicsGetWidth graphics
      height <- graphicsGetHeight graphics
      cameraRay <- cameraGetScreenRay cameraPtr (fromIntegral px / fromIntegral width) (fromIntegral py / fromIntegral height)
      -- Pick only geometry objects, not eg. zones or lights, only get the first (closest) hit
      withObject (cameraRay, RayTriangle, maxDistance, drawableGeometry, defaultViewMask) $ \(query :: Ptr RayOctreeQuery) -> do
        octreeRaycastSingle octree query
        results <- rayOctreeQueryGetResult query
        pure $ if V.null results then Nothing
          else Just $ V.head results
