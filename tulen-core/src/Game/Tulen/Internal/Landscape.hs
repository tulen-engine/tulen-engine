-- | Collected reexports for landscape manipulation
module Game.Tulen.Internal.Landscape(
    module X
  , LoadedLandscape(..)
  , loadLandscape
  , landscapeHeightsFromFunction
  , landscapeTilesFromFunction
  , landscapeUpdateHeights
  , landscapeUpdateTiles
  , updateLoadedLandscape
  ) where

import Control.Lens ((^.))
import Data.Functor.Identity
import Data.Map.Strict (Map)
import Data.Maybe
import Debug.Trace
import Foreign
import Graphics.Urho3D
import Linear

import Game.Tulen.Internal.Landscape.Mesh as X
import Game.Tulen.Internal.Landscape.Texture as X
import Game.Tulen.Internal.Landscape.Types as X

import qualified Data.Array.Repa as R
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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
      loadedLandDatum = l { landscapeUpdatedChunks = mempty }
    , loadedLandNode = landNode
    , loadedLandChunks = landMeshes
    }
  where
    makeMaterial matTemplate landMesh tileSetsArray = do
      -- cmat <- materialClone matTemplate ""
      -- TODO: check why previous line doesn't work at all
      Just (cache :: Ptr ResourceCache) <- getSubsystem app
      Just (cmat' :: Ptr Material) <- cacheGetResource cache "Materials/Landscape.xml" True
      cmat <- materialClone cmat' ""
      materialSetTexture cmat TU'Normal tileSetsArray
      materialSetTexture cmat TU'Diffuse $ landMeshDetails landMesh
      materialSetShaderParameter cmat "ChunkSize" (fromIntegral landscapeChunkSize :: Float)
      pure cmat

    loadChunk :: SharedPtr Material -> Ptr Node -> SharedPtr Texture2DArray -> V2 Int -> LandChunk -> IO LandMesh
    loadChunk matTemplate landNode tileSetsArray (V2 xi yi) chunk = do
      context <- getContext app
      let xneigh  = M.lookup (V2 (xi+1) yi) landscapeChunks
          yneigh  = M.lookup (V2 xi (yi+1)) landscapeChunks
          xyneigh = M.lookup (V2 (xi+1) (yi+1)) landscapeChunks
          mneighbours = (xneigh, yneigh, xyneigh)
      landMesh <- makeLandMesh context (V2 landscapeChunkSize landscapeChunkSize) landscapeTileScale landscapeResolution landscapeVerticalScale mneighbours chunk
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

-- | Set all heights in landscape from function. Coordinates passed in the function are world coordinates and
-- outup height is in world units too.
landscapeHeightsFromFunction :: (V2 Float -> Float) -> Landscape -> Landscape
landscapeHeightsFromFunction f land = land {
    landscapeChunks = M.mapWithKey updChunk $ landscapeChunks land
  , landscapeUpdatedChunks = S.fromList [V2 x y | x <- [lowx .. highx], y <- [lowy .. highy]]
  }
  where
    (V2 lowx lowy, V2 highx highy) = landscapeBounds land
    v2 v = V2 v v
    chunkSize = fmap fromIntegral . v2 $ landscapeChunkSize land
    tileScale =  v2 (landscapeTileScale land)
    chunk2world v = fmap fromIntegral v * tileScale * chunkSize
    updChunk chunkCoord chunk = let
      chunkOrigin = chunk2world chunkCoord
      R.Z R.:. height R.:. width = R.extent $ landChunkHeightmap chunk
      updHeightmap arr = runIdentity . R.computeP . R.traverse arr id $ \_ (R.Z R.:. y' R.:. x) -> let
        y = height - y'
        chunkPoint = chunkOrigin + chunkSize * tileScale * fmap fromIntegral (V2 x y) / fmap fromIntegral (V2 width height)
        in f chunkPoint / landscapeVerticalScale land
      in chunk { landChunkHeightmap = updHeightmap $ landChunkHeightmap chunk}

-- | Set all tiles in landscape from function. Coordinates passed in the function are tile indecies.
-- Output value is tileset index.
landscapeTilesFromFunction :: (V2 Int -> Word8) -> Landscape -> Landscape
landscapeTilesFromFunction f land = land {
    landscapeChunks = M.mapWithKey updChunk $ landscapeChunks land
  , landscapeUpdatedChunks = S.fromList [V2 x y | x <- [lowx .. highx], y <- [lowy .. highy]]
  }
  where
  (V2 lowx lowy, V2 highx highy) = landscapeBounds land
  v2 v = V2 v v
  chunkSize = v2 $ landscapeChunkSize land
  updChunk chunkCoord chunk = chunk { landChunkTiles = updTiles $ landChunkTiles chunk}
    where
    chunkOrigin = chunkCoord * chunkSize
    updTiles arr = runIdentity . R.computeP . R.traverse arr id $ \_ (R.Z R.:. y R.:. x) -> let
      R.Z R.:. height R.:. _ = R.extent arr
      in f $ chunkOrigin + V2 x y

-- | Set all heights in landscape from function. Coordinates passed in the function are world coordinates and
-- outup height is in world units too.
landscapeUpdateHeights :: V2 Int -- ^ Offset of region in tiles
  -> V2 Int -- ^ Size of region in tiles
  -> (V2 Float -> Float -> Float)
  -> Landscape -> Landscape
landscapeUpdateHeights (V2 ox oy) (V2 sx sy) f land = land {
    landscapeChunks = M.mapWithKey updChunk $ landscapeChunks land
  , landscapeUpdatedChunks = S.fromList [V2 x y | x <- [ox' .. ox'+sx'-1], y <- [oy' .. oy'+sy'-1]]
  }
  where
    chsize = landscapeChunkSize land
    ox' = ox `div` chsize
    oy' = oy `div` chsize
    sx' = ceiling $ fromIntegral sx / (fromIntegral chsize :: Float)
    sy' = ceiling $ fromIntegral sy / (fromIntegral chsize :: Float)
    v2 v = V2 v v
    chunkSize = fmap fromIntegral . v2 $ chsize
    tileScale =  v2 (landscapeTileScale land)
    chunk2world v = fmap fromIntegral v * tileScale * chunkSize
    updChunk chunkCoord@(V2 chx chy) chunk
      | chx < ox' || chy < oy' || chx >= ox'+sx' || chy >= oy'+sy' = chunk
      | otherwise = let
          chunkOrigin = chunk2world chunkCoord
          R.Z R.:. height R.:. width = R.extent $ landChunkHeightmap chunk
          updHeightmap arr = runIdentity . R.computeP . R.traverse arr id $ \getter p@(R.Z R.:. y' R.:. x) -> let
            y = height - y'
            chunkPoint = chunkOrigin + chunkSize * tileScale * fmap fromIntegral (V2 x y) / fmap fromIntegral (V2 width height)
            h = getter p * landscapeVerticalScale land
            in if    chunkPoint ^. _x >= fromIntegral ox && chunkPoint ^. _x < fromIntegral (ox+sx)
                  && chunkPoint ^. _y >= fromIntegral oy && chunkPoint ^. _y < fromIntegral (oy+sy)
                then f chunkPoint h / landscapeVerticalScale land
                else getter p
          in chunk { landChunkHeightmap = updHeightmap $ landChunkHeightmap chunk}

-- | Set all heights in landscape from function. Coordinates passed in the function are world coordinates and
-- outup height is in world units too.
landscapeUpdateTiles :: V2 Int -- ^ Offset of region in tiles
  -> V2 Int -- ^ Size of region in tiles
  -> (V2 Int -> Word8 -> Word8)
  -> Landscape -> Landscape
landscapeUpdateTiles (V2 ox oy) (V2 sx sy) f land = land {
    landscapeChunks = M.mapWithKey updChunk $ landscapeChunks land
  , landscapeUpdatedChunks = S.fromList [V2 x y | x <- [ox' .. ox'+sx'-1], y <- [oy' .. oy'+sy'-1]]
  }
  where
    chsize = landscapeChunkSize land
    ox' = ox `div` chsize
    oy' = oy `div` chsize
    sx' = ceiling $ fromIntegral sx / (fromIntegral chsize :: Float)
    sy' = ceiling $ fromIntegral sy / (fromIntegral chsize :: Float)
    v2 v = V2 v v
    chunkSize = fmap fromIntegral . v2 $ chsize
    updChunk chunkCoord@(V2 chx chy) chunk
      | chx < ox' || chy < oy' || chx >= ox'+sx' || chy >= oy'+sy' = chunk
      | otherwise = chunk { landChunkTiles = updTiles $ landChunkTiles chunk}
        where
        chunkOrigin = chunkCoord * chunkSize
        updTiles arr = runIdentity . R.computeP . R.traverse arr id $ \getter p@(R.Z R.:. y R.:. x) -> let
          R.Z R.:. height R.:. _ = R.extent arr
          chunkPoint = chunkOrigin + V2 x y
          in if    chunkPoint ^. _x >= ox && chunkPoint ^. _x < ox+sx
                && chunkPoint ^. _y >= oy && chunkPoint ^. _y < oy+sy
              then f (chunkOrigin + V2 x y) $ getter p
              else getter p

-- | Sync render data for given range of chunks
landscapeUpdateChunk ::
     LoadedLandscape -- ^ Loaded data
  -> V2 Int -- ^ Chunk index
  -> IO ()
landscapeUpdateChunk LoadedLandscape{..} pos = do
  let land = loadedLandDatum
      mchunk = M.lookup pos $ landscapeChunks land
      mmesh = M.lookup pos loadedLandChunks
      mx  = M.lookup (pos + V2 1 0) $ landscapeChunks land
      my  = M.lookup (pos + V2 0 1) $ landscapeChunks land
      mxy = M.lookup (pos + V2 1 1) $ landscapeChunks land
  case (mchunk, mmesh) of
    (Just chunk, Just mesh) -> updateChunkMesh chunk (mx, my, mxy) mesh
    _ -> pure ()

-- | Update loaded landscape and sync changes with engine resources
updateLoadedLandscape :: (Landscape -> Landscape) -> LoadedLandscape -> IO LoadedLandscape
updateLoadedLandscape f ll@LoadedLandscape{..} = do
  let land = f loadedLandDatum
  traverse (landscapeUpdateChunk ll) $ S.elems $ landscapeUpdatedChunks land
  pure ll {
      loadedLandDatum = land { landscapeUpdatedChunks = mempty }
    }
