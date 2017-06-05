{-# LANGUAGE RecursiveDo #-}
-- | Defines main context of engine that encapsulates all resources in one reference.
module Game.Tulen.Internal.Core where

import Data.IORef
import Data.StateVar
import Foreign
import Graphics.Urho3D

-- | Main context of engine. Here goes all referencies to internal resources.
-- Core is used as entry point for game.
data Core = Core {
  coreApplication :: SharedPtr Application
, coreScene       :: SharedPtr Scene
, coreCamera      :: Ptr Node
}

-- | Additional runtime configuration of engine core.
data CoreConfig = CoreConfig {
  -- | Created window title
  coreWindowTitle :: String
  -- | Log file name relative to current folder. Nothing means no log.
, coreLogFile     :: Maybe String
  -- | User can provide additional action for engine setup stage.
, coreCustomSetup :: Maybe (Core -> IO ())
  -- | User can provide additional action for engine startup stage.
, coreCustomStart :: Maybe (Core -> IO ())
-- | User can provide additional action for engine stop stage.
, coreCustomStop :: Maybe (Core -> IO ())
}

-- | Neutral configuration that doesn't affect behavior of engine
defaultCoreConfig :: CoreConfig
defaultCoreConfig = CoreConfig {
    coreWindowTitle = "Tulen engine"
  , coreLogFile = Nothing
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
      }
  applicationRun app

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
  (scene, camera) <- createScene $ coreApplication core
  initMouseMode core MM'Relative
  atomicWriteIORef coreRef $ core {
      coreScene  = scene
    , coreCamera = camera
    }

-- | Internal core stop steps
coreStop :: Core -> IO ()
coreStop Core{..} = do
  eng <- applicationEngine coreApplication
  engineDumpResources eng True

-- | Construct the scene content.
createScene :: SharedPtr Application -> IO (SharedPtr Scene, Ptr Node)
createScene app = do
  cache :: Ptr ResourceCache <- guardJust "Missing subsystem ResourceCache" =<< getSubsystem app
  scene :: SharedPtr Scene <- newSharedObject =<< getContext app

  {-
    Create octree, use default volume (-1000, -1000, -1000) to (1000, 1000, 1000)
  -}
  _ :: Ptr Octree <- guardJust "Failed to create Octree" =<< nodeCreateComponent scene Nothing Nothing

  -- Create a Zone component for ambient lighting & fog control
  zoneNode <- nodeCreateChild scene "Zone" CM'Local 0
  zone :: Ptr Zone <- guardJust "Failed to create Zone" =<< nodeCreateComponent zoneNode Nothing Nothing
  -- Set same volume as the Octree, set a close bluish fog and some ambient light
  zoneSetBoundingBox zone $ BoundingBox (-1000) 1000
  zoneSetFogColor zone $ rgb 0.2 0.2 0.2
  zoneSetFogStart zone 200
  zoneSetFogEnd zone 300

  -- Create a directional light
  lightNode <- nodeCreateChild scene "DirectionalLight" CM'Local 0
  nodeSetDirection lightNode (Vector3 (-0.6) (-1.0) (-0.8)) -- The direction vector does not need to be normalized
  light :: Ptr Light <- guardJust "Failed to create Light" =<< nodeCreateComponent lightNode Nothing Nothing
  lightSetLightType light LT'Directional
  lightSetColor light $ rgb 0.4 1.0 0.4
  lightSetSpecularIntensity light 1.5

  -- Create the camera. Let the starting position be at the world origin. As the fog limits maximum visible distance, we can
  -- bring the far clip plane closer for more effective culling of distant objects
  cameraNode <- nodeCreateChild scene "Camera" CM'Local 0
  nodeSetPosition cameraNode (Vector3 0 2 (-20))
  cam :: Ptr Camera <- guardJust "Failed to create Camera" =<< nodeCreateComponent cameraNode Nothing Nothing
  cameraSetFarClip cam 300

  return (scene, cameraNode)

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
