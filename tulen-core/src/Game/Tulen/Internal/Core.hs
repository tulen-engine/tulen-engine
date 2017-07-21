{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf #-}
-- | Defines main context of engine that encapsulates all resources in one reference.
module Game.Tulen.Internal.Core where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens ((^.))
import Data.IORef
import Data.StateVar
import Foreign hiding (void)
import Graphics.Urho3D
import Graphics.Urho3D.Multithread
import Paths_tulen_core
import System.Directory

import Game.Tulen.Internal.Camera
import Game.Tulen.Internal.ExternalRef
import Game.Tulen.Internal.Utils

-- DEBUG
import Debug.Trace
import Game.Tulen.Internal.Landscape
import Linear
import qualified Data.Array.Repa as R
import qualified Data.Map.Strict as M
-- end DEBUG

import Game.Tulen.Internal.Core.Types
import Game.Tulen.Internal.Monad

-- | Execute game with custom initialisation action. Blocks until exit.
runCore :: CoreConfig -- ^ Hooks and values that allow to configure engine.
  -> TulenM () -- ^ Reactive network to execute
  -> IO ()
runCore cfg@CoreConfig{..} m = withObject () $ \context -> do
  rec
    let withCore f = readIORef coreRef >>= f
    app <- newSharedObject (context
      , withCore (coreSetup cfg) >> maybe (pure ()) withCore coreCustomSetup
      , coreStart m coreRef >> maybe (pure ()) withCore coreCustomStart
      , withCore coreStop >> maybe (pure ()) withCore coreCustomStop)
    camerasVar <- newTVarIO (mempty, 0)
    landChan <- newTChanIO
    coreRef <- newIORef Core {
        coreApplication = app
      , coreScene = error "Scene not initialized at startup"
      , coreCameras = camerasVar
      , coreActiveCamera = error "Camera variable is accessible only inside reactive monad"
      , coreConfig = cfg
      , coreUI = error "UI not initialized at startup"
      , coreInput = error "Input not initialized at startup"
      , coreResourceCache = error "ResourceCache not initialized at startup"
      , coreFileSystem = error "FileSystem not initialized at startup"
      , coreOctree = error "Octree not initialized at startup"
      , coreGraphics = error "Graphics not initialized at startup"
      , coreCursor = error "Cursor not initalized at startup"
      , coreRenderer = error "Renderer not initialized at startup"
      , coreLandscape = error "Landscape variable is accessible only inside reactive monad"
      , coreLandscapeChan = landChan
      }
  applicationRun app

-- | Try to guess resource dir with core assets
getResourceDir :: MonadIO m => Maybe FilePath -> m FilePath
getResourceDir (Just p) = pure p
getResourceDir Nothing = liftIO $ do
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
coreStart :: TulenM () -> IORef Core -> IO ()
coreStart m coreRef = do
  core <- readIORef coreRef
  let app = coreApplication core
  ui <- guardJust "UI" =<< getSubsystem app
  input <- guardJust "Input" =<< getSubsystem app
  fileSystem <- guardJust "FileSystem" =<< getSubsystem app
  cache <- guardJust "ResourceCache" =<< getSubsystem app
  graphics <- guardJust "Graphics" =<< getSubsystem app
  renderer <- guardJust "Renderer" =<< getSubsystem app
  let coreWithSystems1 = core {
      coreUI = ui
    , coreInput = input
    , coreResourceCache = cache
    , coreFileSystem = fileSystem
    , coreGraphics = graphics
    , coreRenderer = renderer
    }
  atomicWriteIORef coreRef coreWithSystems1

  (loadedLand, scene, cid) <- flip runReaderT coreWithSystems1 $ do
    initResources (coreConfig core)
    (scene, cid, loadedLand) <- createScene
    octree <- guardJust "Octree" =<< nodeGetComponent scene True
    local (\c -> c { coreScene = scene, coreOctree = octree }) $ do
      createUI
      cursor <- guardJust "Cursor" . wrapNullPtr =<< uiCursor ui
      local (\c -> c { coreCursor = cursor }) $ do
        setupViewport cid
        subscribeToEvents cid
        initMouseMode MM'Relative
        pure (loadedLand, scene, cid)

  octree <- guardJust "Octree" =<< nodeGetComponent scene True
  cursor <- guardJust "Cursor" . wrapNullPtr =<< uiCursor ui
  let coreWithSystems2 = coreWithSystems1 {
      coreScene = scene
    , coreOctree = octree
    , coreCursor = cursor
    }
  atomicWriteIORef coreRef coreWithSystems2
  _ <- forkOS $ runTulenM coreWithSystems2 $ do
    camVar <- newExternalRef cid
    landVar <- newExternalRef loadedLand
    -- start thread that aply land patches
    _ <- liftIO . forkIO . forever $ do
      (landPatch, patchNotify) <- atomically . readTChan $ coreLandscapeChan coreWithSystems2
      runInMainThread $ do
        modifyExternalRefM landVar $ fmap (, ()) . updateLoadedLandscape landPatch
        patchNotify
    -- execute user reactive network
    local (\c -> c {
        coreActiveCamera = camVar
      , coreLandscape = landVar
      }) m
  pure ()

initResources :: (MonadReader Core m, MonadIO m) => CoreConfig -> m ()
initResources CoreConfig{..} = do
  app <- asks coreApplication
  cache <- asks coreResourceCache
  path <- getResourceDir coreResources
  _ <- cacheAddResourceDir cache path priorityLast
  pure ()

-- | Internal core stop steps
coreStop :: Core -> IO ()
coreStop Core{..} = do
  eng <- applicationEngine coreApplication
  engineDumpResources eng True

-- | Construct the scene content.
createScene :: (MonadReader Core m, MonadIO m) => m (SharedPtr Scene, CameraId, LoadedLandscape)
createScene = do
  app <- asks coreApplication
  context <- getContext app
  cache <- asks coreResourceCache
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
  camera <- guardJust "Failed to create camera" =<< newCamera cameraNode
  cameraSetFarClip (cameraPtr camera) 300

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
  loadedLand <- loadLandscape (parentPointer scene) landscape
  -- END DEBUG
  --------

  return (scene, cameraId camera, loadedLand)

-- | Set up a viewport for displaying the scene.
setupViewport :: (MonadIO m, MonadReader Core m) => CameraId -> m ()
setupViewport cid = do
  renderer <- asks coreRenderer

  {-
    Set up a viewport to the Renderer subsystem so that the 3D scene can be seen. We need to define the scene and the camera
    at minimum. Additionally we could configure the viewport screen size and the rendering path (eg. forward / deferred) to
    use, but now we just use full screen and default render path configured in the engine command line options
  -}
  cntx <- getContext =<< asks coreApplication
  void . withCamera cid $ \CameraData{..} -> do
    scene <- asks coreScene
    cnts <- getContext =<< asks coreApplication
    (viewport :: SharedPtr Viewport) <- newSharedObject (cntx, pointer scene, cameraPtr)
    rendererSetViewport renderer 0 viewport


-- | Create default UI
createUI :: (MonadIO m, MonadReader Core m) => m ()
createUI = do
  cache <- asks coreResourceCache
  ui <- asks coreUI
  roote <- uiRoot ui
  context <- getContext =<< asks coreApplication

  -- Create a Cursor UI element because we want to be able to hide and show it at will. When hidden, the mouse cursor will
  -- control the camera, and when visible, it will point the raycast target
  style :: Ptr XMLFile <- guardJust "DefaultStyle.xml" =<< cacheGetResource cache "UI/DefaultStyle.xml" True
  cursor :: SharedPtr Cursor <- newSharedObject context
  uiElementSetStyleAuto cursor style
  uiSetCursor ui $ pointer cursor
  uiElementSetVisible cursor True

-- | Subscribe to application-wide logic update events.
subscribeToEvents :: (MonadIO m, MonadReader Core m) => CameraId -> m ()
subscribeToEvents cid = do
  app <- asks coreApplication
  core <- ask
  subscribeToEvent app $ flip runReaderT core . handleUpdate cid
  subscribeToEvent app $ flip runReaderT core . handlePostRenderUpdate cid

-- | Handle the logic update event.
handleUpdate :: (MonadIO m, MonadReader Core m) => CameraId -> EventUpdate -> m ()
handleUpdate cid e = do
  -- Take the frame time step, which is stored as a float
  let t = e ^. timeStep
  -- Move the camera, scale movement with time step
  updateCamera cid $ moveCamera t

handlePostRenderUpdate :: (MonadIO m, MonadReader Core m) => CameraId ->  EventPostRenderUpdate -> m ()
handlePostRenderUpdate cid _ =
  -- If draw debug mode is enabled, draw viewport debug geometry, which will show eg. drawable bounding boxes and skeleton
  -- bones. Note that debug geometry has to be separately requested each frame. This time use depth test, as otherwise the result becomes
  -- hard to interpret due to large object count
  void $ withCamera cid $ \camData -> do
    renderer <- asks coreRenderer
    when (cameraDebugGeometry camData) $ rendererDrawDebugGeometry renderer True

-- | Change mouse visibility and behavior
initMouseMode :: (MonadIO m, MonadReader Core m) => MouseMode -> m ()
initMouseMode mode = do
  input <- asks coreInput
  when (mode == MM'Free) $ inputSetMouseVisible input True False
  when (mode /= MM'Absolute) $ inputSetMouseMode input mode False
