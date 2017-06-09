{-# LANGUAGE RecursiveDo #-}
-- | Defines main context of engine that encapsulates all resources in one reference.
module Game.Tulen.Internal.Core where

import Control.Lens ((^.))
import Data.IORef
import Data.StateVar
import Foreign
import Graphics.Urho3D
import Paths_tulen_core
import System.Directory

-- DEBUG
import Game.Tulen.Internal.Landscape
import qualified Data.Array.Repa as R
import Debug.Trace
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
  (scene, camera) <- createScene $ coreApplication core
  setupViewport app scene camera
  subscribeToEvents app camera
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
createScene :: SharedPtr Application -> IO (SharedPtr Scene, Ptr Node)
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
  let chsize = 10
      res = 30
      chunk0 = emptyLandChunk chsize 0 res 1000
      initHeights arr = R.computeS $ R.traverse arr id $ \getter (R.Z R.:. y R.:. x) -> let
        x' = 0.005 * fromIntegral x
        y' = 0.005 * fromIntegral y
        d = x'^2 + y'^2
        in 0.00025 * (1 + sin d)
      chunk = chunk0 { landChunkHeightmap = initHeights $ landChunkHeightmap chunk0 }
  landMesh <- makeLandMesh context chsize 1 res 1000 chunk
  let model = landMeshModel landMesh
  node <- nodeCreateChild scene "FromScratchObject" CM'Replicated 0
  nodeSetPosition node $ Vector3 0 0 0
  object :: Ptr StaticModel <- guardJust "static model for debug" =<< nodeCreateComponent node Nothing Nothing
  staticModelSetModel object model
  (planeMaterial :: Ptr Material) <- guardJust "Landscape.xml" =<< cacheGetResource cache "Materials/Landscape.xml" True
  staticModelSetMaterial object planeMaterial
  drawableSetCastShadows object True
  -- END DEBUG
  --------

  return (scene, cameraNode)

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

-- | Read input and moves the camera.
moveCamera :: SharedPtr Application -> Ptr Node -> Float -> CameraData -> IO CameraData
moveCamera app cameraNode t camData = do
  (ui :: Ptr UI) <- guardJust "UI" =<< getSubsystem app

  -- Do not move if the UI has a focused element (the console)
  mFocusElem <- uiFocusElement ui
  whenNothing mFocusElem camData $ do
    (input :: Ptr Input) <- guardJust "Input" =<< getSubsystem app

    -- Movement speed as world units per second
    let moveSpeed = 20
    -- Mouse sensitivity as degrees per pixel
    let mouseSensitivity = 0.1

    -- Use this frame's mouse motion to adjust camera node yaw and pitch. Clamp the pitch between -90 and 90 degrees
    mouseMove <- inputGetMouseMove input
    let yaw = camYaw camData + mouseSensitivity * fromIntegral (mouseMove ^. x)
    let pitch = clamp (-90) 90 $ camPitch camData + mouseSensitivity * fromIntegral (mouseMove ^. y)

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
subscribeToEvents :: SharedPtr Application -> Ptr Node -> IO ()
subscribeToEvents app cameraNode = do
  camDataRef <- newIORef $ CameraData 0 30 False
  timeRef <- newIORef 0
  subscribeToEvent app $ handleUpdate app cameraNode camDataRef timeRef
  subscribeToEvent app $ handlePostRenderUpdate app camDataRef

-- | Handle the logic update event.
handleUpdate :: SharedPtr Application -> Ptr Node -> IORef CameraData -> IORef Float -> EventUpdate -> IO ()
handleUpdate app cameraNode camDataRef timeRef e = do
  -- Take the frame time step, which is stored as a float
  let t = e ^. timeStep
  camData <- readIORef camDataRef
  -- Move the camera, scale movement with time step
  writeIORef camDataRef =<< moveCamera app cameraNode t camData

handlePostRenderUpdate :: SharedPtr Application -> IORef CameraData -> EventPostRenderUpdate -> IO ()
handlePostRenderUpdate app camDataRef _ = do
  camData <- readIORef camDataRef
  (renderer :: Ptr Renderer) <- guardJust "Renderer" =<< getSubsystem app

  -- If draw debug mode is enabled, draw viewport debug geometry, which will show eg. drawable bounding boxes and skeleton
  -- bones. Note that debug geometry has to be separately requested each frame. This time use depth test, as otherwise the result becomes
  -- hard to interpret due to large object count
  when (camDebugGeometry camData) $
    rendererDrawDebugGeometry renderer True

-- | Change mouse visibility and behavior
initMouseMode :: Core -> MouseMode -> IO ()
initMouseMode core mode = do
  let app = coreApplication core
  input :: Ptr Input <- guardJust "Missing system InputSystem" =<< getSubsystem app
  when (mode == MM'Free) $ inputSetMouseVisible input True False
  when (mode /= MM'Absolute) $ inputSetMouseMode input mode False

-- | Fail with helpfull message when encounter 'Nothing'
guardJust :: String -> Maybe a -> IO a
guardJust msg Nothing = fail $ "Unrecoverable error! " ++ msg
guardJust _ (Just a) = pure a

-- | Helper to run code when value is nothing
whenNothing :: Monad m => Maybe a -> b -> m b -> m b
whenNothing Nothing _ f = f
whenNothing (Just _) a _ = return a

clamp :: Ord a => a -> a -> a -> a
clamp mina maxa = max mina . min maxa
