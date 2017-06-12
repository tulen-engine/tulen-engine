-- | Collected reexports for landscape manipulation
module Game.Tulen.Internal.Landscape(
    module X
  , LoadedLandscape(..)
  , loadLandscape
  ) where

import Data.Map.Strict (Map)
import Foreign
import Graphics.Urho3D
import Linear

import Game.Tulen.Internal.Landscape.Mesh as X
import Game.Tulen.Internal.Landscape.Texture as X
import Game.Tulen.Internal.Landscape.Types as X

import qualified Data.Map.Strict as M

-- | Landscape alongside with all objects that were generated for rendering
data LoadedLandscape = LoadedLandscape {
  -- | The original data of landscape, when this changes, need regenerate engine
  -- derivitives.
  loadedLandDatum  :: !Landscape
  -- | Root scene node for landscape, all chunks are connected to it.
, loadedLandNode   :: !(Ptr Node)
  -- | Runtime data that you need to upgrade after modifying original landscape data.
, loadedLandChunks :: !(Map (V2 Int) LandMesh)
}

-- | Create landscape from description and attach it to scene.
loadLandscape :: SharedPtr Application -- ^ Application reference
  -> Ptr Node -- ^ Scene or other node to attach landscape to
  -> Landscape -- ^ Landscape data
  -> IO LoadedLandscape
loadLandscape app scene l@Landscape{..} = do
  putStrLn "Making material..."
  matTemplate <- makeLandscapeMaterial app
  putStrLn "Making atlases..."
  tileSetsArray <- makeTilesTexture app landscapeTiles
  putStrLn "Making chunks..."
  landNode <- nodeCreateChild scene "Landscape" CM'Local 0
  landMeshes <- sequence $ M.mapWithKey (loadChunk matTemplate landNode tileSetsArray) landscapeChunks
  putStrLn "Done."
  pure LoadedLandscape {
      loadedLandDatum = l
    , loadedLandNode = landNode
    , loadedLandChunks = landMeshes
    }
  where
    makeMaterial matTemplate landMesh tileSetsArray = do
      -- cmat <- materialClone matTemplate ""
      -- TODO: check why previous line doesn't work at all
      Just (cache :: Ptr ResourceCache) <- getSubsystem app
      Just (cmat :: Ptr Material) <- cacheGetResource cache "Materials/Landscape.xml" True
      materialSetTexture cmat TU'Normal tileSetsArray
      materialSetTexture cmat TU'Diffuse $ landMeshDetails landMesh
      pure cmat

    loadChunk :: SharedPtr Material -> Ptr Node -> SharedPtr Texture2DArray -> V2 Int -> LandChunk -> IO LandMesh
    loadChunk matTemplate landNode tileSetsArray (V2 xi yi) chunk = do
      context <- getContext app
      landMesh <- makeLandMesh context (V2 landscapeChunkSize landscapeChunkSize) landscapeTileScale landscapeResolution landscapeVerticalScale Nothing chunk
      let model = landMeshModel landMesh
          name = "LandChunk_" ++ show xi ++ "_" ++ show yi
      node <- nodeCreateChild landNode name CM'Local 0
      let toPos v = fromIntegral (v * landscapeChunkSize) * landscapeTileScale
      nodeSetPosition node $ Vector3 (toPos xi) 0 (toPos yi)
      mobject <- nodeCreateComponent node Nothing Nothing
      case mobject of
        Nothing -> fail $ "Failed to create chunk static model for " ++ name -- TODO: error handling
        Just (object :: Ptr StaticModel) -> do
          staticModelSetModel object model
          material <- makeMaterial matTemplate landMesh tileSetsArray
          staticModelSetMaterial object material
          drawableSetCastShadows object True
          pure landMesh
