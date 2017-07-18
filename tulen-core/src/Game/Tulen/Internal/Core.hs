{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf #-}
-- | Defines main context of engine that encapsulates all resources in one reference.
module Game.Tulen.Internal.Core where

import Control.Concurrent
import Control.Lens ((^.))
import Data.IORef
import Data.StateVar
import Foreign hiding (void)
import Graphics.Urho3D
import Paths_tulen_core
import System.Directory

import Game.Tulen.Internal.Camera
import Game.Tulen.Internal.Utils

-- DEBUG
import Debug.Trace
import Game.Tulen.Internal.Landscape
import Linear
import qualified Data.Array.Repa as R
import qualified Data.Map.Strict as M
-- end DEBUG

-- | Main context of engine. Here goes all referencies to internal resources.
-- Core is used as entry point for game.
data Core = Core {
  coreApplication :: SharedPtr Application
, coreScene       :: SharedPtr Scene
, coreCamera      :: Ptr Node
, coreConfig      :: CoreConfig
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

-- | Execute game with custom initialisation action.
runCore :: CoreConfig -- ^ Hooks and values that allow to configure engine.
  -> IO ()
runCore cfg@CoreConfig{..} = withObject () $ \context -> do
  rec
    let withCore f = readIORef coreRef >>= f
    app <- newSharedObject (context
      , withCore (coreSetup cfg) >> maybe (pure ()) withCore coreCustomSetup
      , coreStart coreRef >> maybe (pure ()) withCore coreCustomStart
      , withCore coreStop >> maybe (pure ()) withCore coreCustomStop)
    coreRef <- newIORef Core {
        coreApplication = app
      , coreScene = undefined -- initialized at coreStart
      , coreCamera = undefined
      , coreConfig = cfg
      }
  applicationRun app

-- | Try to guess resource dir with core assets
getResourceDir :: Maybe FilePath -> IO FilePath
getResourceDir (Just p) = pure p
getResourceDir Nothing = do
  p <- getDataFileName "Data"
  exists <- doesDirectoryExist p
  pure $ if not exists then "./Data" else p

-- | Internal core setup steps
coreSetup :: CoreConfig -> Core -> IO ()
coreSetup CoreConfig{..} core = do
  let app = coreApplication core
  startupParameter app "WindowTitle" $= coreWindowTitle

  fs :: Ptr FileSystem <- guardJust "Cannot find subsystem FileSystem" =<< getSubsystem app
  prefDir <- getAppPreferencesDir fs "urho3d" "logs"
  whenJust coreLogFile $ \f -> startupParameter app "LogName" $= prefDir ++ "/" ++ f

  startupParameter app "FullScreen" $= False
  startupParameter app "Headless" $= False
  startupParameter app "ForceGL2" $= True
  startupParameter app "Sound" $= False -- TODO: remove this

-- | Internal core run steps
coreStart :: IORef Core -> IO ()
coreStart coreRef = do
  core <- readIORef coreRef
  let app = coreApplication core
  initResources (coreConfig core) app
  (scene, camera, loadedLand) <- createScene $ coreApplication core
  createUI app
  setupViewport app scene camera
  loadedLandRef <- newIORef loadedLand
  subscribeToEvents app camera loadedLandRef
  initMouseMode core MM'Relative
  atomicWriteIORef coreRef $ core {
      coreScene  = scene
    , coreCamera = camera
    }

initResources :: CoreConfig -> SharedPtr Application -> IO ()
initResources CoreConfig{..} app = do
  path <- getResourceDir coreResources
  cache :: Ptr ResourceCache <- guardJust "ResourceCache" =<< getSubsystem app
  _ <- cacheAddResourceDir cache path priorityLast
  pure ()

-- | Internal core stop steps
coreStop :: Core -> IO ()
coreStop Core{..} = do
  eng <- applicationEngine coreApplication
  engineDumpResources eng True

-- | Construct the scene content.
createScene :: SharedPtr Application -> IO (SharedPtr Scene, Ptr Node, LoadedLandscape)
createScene app = do
  context :: Ptr Context <- getContext app
  cache :: Ptr ResourceCache <- guardJust "Missing subsystem ResourceCache" =<< getSubsystem app
  scene :: SharedPtr Scene <- newSharedObject context

  {-
    Create octree, use default volume (-1000, -1000, -1000) to (1000, 1000, 1000)
  -}
  _ :: Ptr Octree <- guardJust "Failed to create Octree" =<< nodeCreateComponent scene Nothing Nothing

  -- Create a Zone component for ambient lighting & fog control
  zoneNode <- nodeCreateChild scene "Zone" CM'Local 0
  zone :: Ptr Zone <- guardJust "Failed to create Zone" =<< nodeCreateComponent zoneNode Nothing Nothing
  -- Set same volume as the Octree, set a close bluish fog and some ambient light
  zoneSetBoundingBox zone $ BoundingBox (-1000) 1000
  zoneSetAmbientColor zone $ rgb 0.15 0.15 0.15
  zoneSetFogColor zone $ rgb 0.2 0.2 0.2
  zoneSetFogStart zone 200
  zoneSetFogEnd zone 300

  -- Create a directional light
  lightNode <- nodeCreateChild scene "DirectionalLight" CM'Local 0
  nodeSetDirection lightNode (Vector3 0.5 (-1.0) 0.1) -- The direction vector does not need to be normalized
  light :: Ptr Light <- guardJust "Failed to create Light" =<< nodeCreateComponent lightNode Nothing Nothing
  lightSetLightType light LT'Directional
  drawableSetCastShadows light True
  lightSetShadowBias light $ BiasParameters 0.00025 0.5
  -- Set cascade splits at 10, 50 and 200 world units, fade shadows out at 80% of maximum shadow distance
  lightSetShadowCascade light $ CascadeParameters 10 50 200 0 0.8 1.0

  -- Create the camera. Let the starting position be at the world origin. As the fog limits maximum visible distance, we can
  -- bring the far clip plane closer for more effective culling of distant objects
  cameraNode <- nodeCreateChild scene "Camera" CM'Local 0
  nodeSetPosition cameraNode (Vector3 2 2 2)
  cam :: Ptr Camera <- guardJust "Failed to create Camera" =<< nodeCreateComponent cameraNode Nothing Nothing
  cameraSetFarClip cam 300

  --------
  -- DEBUG
  let res = 10
      initTiles v@(V2 x y) =
        if
           | x == 1 && y == 1 -> 1
           | x == 2 && y == 2 -> 1
           | x == 3 && y == 2 -> 1
           | x == 3 && y == 1 -> 2
           | x == 4 && y == 1 -> 2
           | x == 4 && y == 2 -> 2
           | x == 1 && y == 3 -> 6
           | x == 2 && y == 3 -> 6
           | x == 3 && y == 3 -> 6
           | x == 1 && y == 1 -> 6
           | x == 32 -> 2
           | y == 16 -> 3
           | otherwise -> 0
      tileSets = [
          TileInfo "Textures/Barrens/Barrens_Dirt.png"
        , TileInfo "Textures/Barrens/Barrens_DirtRough.png"
        , TileInfo "Textures/Barrens/Barrens_Grass.png"
        , TileInfo "Textures/Barrens/Barrens_DirtGrass.png"
        , TileInfo "Textures/Barrens/Barrens_Pebbles.png"
        , TileInfo "Textures/Barrens/Barrens_Rock.png"
        ]
      initHeights (V2 x y) = 1 + sin (0.002 * (x^2 + y^2))
      landscape = landscapeTilesFromFunction initTiles $ landscapeHeightsFromFunction initHeights $ (emptyLandscape 10) {
          landscapeTiles  = tileSets
        , landscapeResolution = res
        }
  loadedLand <- loadLandscape app (parentPointer scene) landscape
  -- END DEBUG
  --------

  return (scene, cameraNode, loadedLand)

-- | Set up a viewport for displaying the scene.
setupViewport :: SharedPtr Application -> SharedPtr Scene -> Ptr Node -> IO ()
setupViewport app scene cameraNode = do
  (renderer :: Ptr Renderer) <- guardJust "Renderer" =<< getSubsystem app

  {-
    Set up a viewport to the Renderer subsystem so that the 3D scene can be seen. We need to define the scene and the camera
    at minimum. Additionally we could configure the viewport screen size and the rendering path (eg. forward / deferred) to
    use, but now we just use full screen and default render path configured in the engine command line options
  -}
  cntx <- getContext app
  (cam :: Ptr Camera) <- guardJust "Camera" =<< nodeGetComponent cameraNode False
  (viewport :: SharedPtr Viewport) <- newSharedObject (cntx, pointer scene, cam)
  rendererSetViewport renderer 0 viewport

data CameraData = CameraData {
  camYaw :: Float
, camPitch :: Float
, camDebugGeometry :: Bool
}

-- | Create default UI
createUI :: SharedPtr Application -> IO ()
createUI app = do
  cache :: Ptr ResourceCache <- guardJust "ResourceCache" =<< getSubsystem app
  ui :: Ptr UI <- guardJust "UI" =<< getSubsystem app
  roote <- uiRoot ui
  context <- getContext app

  -- Create a Cursor UI element because we want to be able to hide and show it at will. When hidden, the mouse cursor will
  -- control the camera, and when visible, it will point the raycast target
  style :: Ptr XMLFile <- guardJust "DefaultStyle.xml" =<< cacheGetResource cache "UI/DefaultStyle.xml" True
  cursor :: SharedPtr Cursor <- newSharedObject context
  uiElementSetStyleAuto cursor style
  uiSetCursor ui $ pointer cursor
  uiElementSetVisible cursor True

-- | Read input and moves the camera.
moveCamera :: SharedPtr Application -> Ptr Node -> Float -> CameraData -> IO CameraData
moveCamera app cameraNode t camData = do
  -- Right mouse button controls mouse cursor visibility: hide when pressed
  ui :: Ptr UI <- guardJust "UI" =<< getSubsystem app
  input :: Ptr Input <- guardJust "Input" =<< getSubsystem app
  cursor <- uiCursor ui
  isRightPress <- inputGetMouseButtonDown input mouseButtonRight
  uiElementSetVisible cursor $ not isRightPress

  -- Do not move if the UI has a focused element (the console)
  mFocusElem <- uiFocusElement ui
  whenNothing mFocusElem camData $ do
    (input :: Ptr Input) <- guardJust "Input" =<< getSubsystem app

    -- Movement speed as world units per second
    let moveSpeed = 20
    -- Mouse sensitivity as degrees per pixel
    let mouseSensitivity = 0.1

    -- Use this frame's mouse motion to adjust camera node yaw and pitch. Clamp the pitch between -90 and 90 degrees
    isVisible <- uiElementIsVisible cursor
    (yaw, pitch) <- if not isVisible then do
        mouseMove <- inputGetMouseMove input
        let yaw = camYaw camData + mouseSensitivity * fromIntegral (mouseMove ^. x)
        let pitch = clamp (-90) 90 $ camPitch camData + mouseSensitivity * fromIntegral (mouseMove ^. y)

        -- Construct new orientation for the camera scene node from yaw and pitch. Roll is fixed to zero
        nodeSetRotation cameraNode $ quaternionFromEuler pitch yaw 0
        pure (yaw, pitch)
      else pure (camYaw camData, camPitch camData)

    -- Construct new orientation for the camera scene node from yaw and pitch. Roll is fixed to zero
    nodeSetRotation cameraNode $ quaternionFromEuler pitch yaw 0

    -- Read WASD keys and move the camera scene node to the corresponding direction if they are pressed
    -- Use the Translate() function (default local space) to move relative to the node's orientation.
    whenM (inputGetKeyDown input KeyW) $
      nodeTranslate cameraNode (vec3Forward `mul` (moveSpeed * t)) TS'Local
    whenM (inputGetKeyDown input KeyS) $
      nodeTranslate cameraNode (vec3Back `mul` (moveSpeed * t)) TS'Local
    whenM (inputGetKeyDown input KeyA) $
      nodeTranslate cameraNode (vec3Left `mul` (moveSpeed * t)) TS'Local
    whenM (inputGetKeyDown input KeyD) $
      nodeTranslate cameraNode (vec3Right `mul` (moveSpeed * t)) TS'Local

    -- Toggle debug geometry with space
    spacePressed <- inputGetKeyPress input KeySpace

    return camData {
        camYaw = yaw
      , camPitch = pitch
      , camDebugGeometry = (if spacePressed then not else id) $ camDebugGeometry camData
      }
  where
    mul (Vector3 a b c) v = Vector3 (a*v) (b*v) (c*v)

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: SharedPtr Application -> Ptr Node -> IORef LoadedLandscape -> IO ()
subscribeToEvents app cameraNode loadedLandRef = do
  camDataRef <- newIORef $ CameraData 0 30 False
  timeRef <- newIORef 0
  subscribeToEvent app $ handleUpdate app cameraNode camDataRef timeRef loadedLandRef
  subscribeToEvent app $ handlePostRenderUpdate app camDataRef
  subscribeToEvent app $ handleMouseDown app loadedLandRef cameraNode

-- | Handle the logic update event.
handleUpdate :: SharedPtr Application -> Ptr Node -> IORef CameraData -> IORef Float -> IORef LoadedLandscape -> EventUpdate -> IO ()
handleUpdate app cameraNode camDataRef timeRef loadedLandRef e = do
  -- Take the frame time step, which is stored as a float
  let t = e ^. timeStep
  camData <- readIORef camDataRef
  -- Move the camera, scale movement with time step
  writeIORef camDataRef =<< moveCamera app cameraNode t camData

  -- DEBUG
  (input :: Ptr Input) <- guardJust "Input" =<< getSubsystem app
  whenM (inputGetKeyPress input KeySpace) $ do
    loadedLand <- readIORef loadedLandRef
    writeIORef loadedLandRef =<< updateLoadedLandscape (landscapeAddCircleHeights 15 7 0.1) loadedLand
    -- writeIORef loadedLandRef =<< updateLoadedLandscape (landscapeUpdateHeights 10 7 (const (+0.1))) loadedLand

  -- DEBUG END
handlePostRenderUpdate :: SharedPtr Application -> IORef CameraData -> EventPostRenderUpdate -> IO ()
handlePostRenderUpdate app camDataRef _ = do
  camData <- readIORef camDataRef
  (renderer :: Ptr Renderer) <- guardJust "Renderer" =<< getSubsystem app

  -- If draw debug mode is enabled, draw viewport debug geometry, which will show eg. drawable bounding boxes and skeleton
  -- bones. Note that debug geometry has to be separately requested each frame. This time use depth test, as otherwise the result becomes
  -- hard to interpret due to large object count
  when (camDebugGeometry camData) $
    rendererDrawDebugGeometry renderer True

handleMouseDown :: SharedPtr Application -> IORef LoadedLandscape -> Ptr Node -> MouseButtonDown -> IO ()
handleMouseDown app loadedLandRef camNode MouseButtonDown{..}
  | mouseButtonDownButton == mouseButtonLeft = updateLand $ \x z -> landscapeUpdateTiles (round <$> V2 x z) 1 (\_ _ -> 3)
  | mouseButtonDownButton == mouseButtonRight = updateLand $ \x z -> landscapeUpdateTiles (round <$> V2 x z) 1 (\_ _ -> 1)
  | otherwise = pure ()
  where
    updateLand f = do
      camera :: Ptr Camera <- guardJust "Camera" =<< nodeGetComponent camNode True
      mres <- cursorRaycastSingle app camera 250 -- hangs here
      whenJust mres $ \RayQueryResult{..} -> do
        loadedLand <- readIORef loadedLandRef
        let Vector3 x _ z = _rayQueryResultPosition
        -- writeIORef loadedLandRef =<< updateLoadedLandscape (landscapeAddCircleHeights (V2 x z) 7 0.1) loadedLand
        writeIORef loadedLandRef =<< updateLoadedLandscape (f x z) loadedLand
        pure ()
      pure ()
-- | Change mouse visibility and behavior
initMouseMode :: Core -> MouseMode -> IO ()
initMouseMode core mode = do
  let app = coreApplication core
  input :: Ptr Input <- guardJust "Missing system InputSystem" =<< getSubsystem app
  when (mode == MM'Free) $ inputSetMouseVisible input True False
  when (mode /= MM'Absolute) $ inputSetMouseMode input mode False
