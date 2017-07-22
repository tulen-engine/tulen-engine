-- | Collected reexports for landscape manipulation
module Game.Tulen.Internal.Landscape(
    module X
  , LoadedLandscape(..)
  , loadLandscape
  , landscapeHeightsFromFunction
  , landscapeTilesFromFunction
  , landscapeUpdateHeights
  , landscapeAddCircleHeights
  , landscapeUpdateTiles
  , updateLoadedLandscape
  , landscapeGetTile
  , landscapeGetTileVariance
  , landscapeGetHeight
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Map.Strict (Map)
import Data.Maybe
import Foreign
import Graphics.Urho3D
import Linear

import Game.Tulen.Internal.Core.Types
import Game.Tulen.Internal.Landscape.Mesh as X
import Game.Tulen.Internal.Landscape.Texture as X
import Game.Tulen.Internal.Landscape.Types as X

import qualified Data.Array.Repa as R
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Debug.Trace

-- | Create landscape from description and attach it to scene.
loadLandscape :: forall m . (MonadIO m, MonadReader Core m)
  => Ptr Node -- ^ Scene or other node to attach landscape to
  -> Landscape -- ^ Landscape data
  -> m LoadedLandscape
loadLandscape scene l@Landscape{..} = do
  liftIO . putStrLn $ "Making material..."
  matTemplate <- makeLandscapeMaterial
  liftIO . putStrLn $ "Making atlases..."
  tileSetsArray <- makeTilesTexture landscapeTiles
  liftIO . putStrLn $ "Making chunks..."
  landNode <- nodeCreateChild scene "Landscape" CM'Local 0
  landMeshes <- sequence $ M.mapWithKey (loadChunk matTemplate landNode tileSetsArray) landscapeChunks
  liftIO . putStrLn $ "Done."
  pure LoadedLandscape {
      loadedLandDatum = l { landscapeUpdatedChunks = mempty }
    , loadedLandNode = landNode
    , loadedLandChunks = landMeshes
    }
  where
    makeMaterial matTemplate landMesh tileSetsArray = do
      -- cmat <- materialClone matTemplate ""
      -- TODO: check why previous line doesn't work at all
      cache <- asks coreResourceCache
      Just (cmat' :: Ptr Material) <- cacheGetResource cache "Materials/Landscape.xml" True
      cmat <- materialClone cmat' ""
      materialSetTexture cmat TU'Normal tileSetsArray
      materialSetTexture cmat TU'Diffuse $ landMeshDetails landMesh
      materialSetShaderParameter cmat "ChunkSize" (fromIntegral landscapeChunkSize :: Float)
      pure cmat

    loadChunk :: SharedPtr Material -> Ptr Node -> SharedPtr Texture2DArray -> V2 Int -> LandChunk -> m LandMesh
    loadChunk matTemplate landNode tileSetsArray (V2 xi yi) chunk = do
      context <- getContext =<< asks coreApplication
      let xneigh  = M.lookup (V2 (xi+1) yi) landscapeChunks
          yneigh  = M.lookup (V2 xi (yi+1)) landscapeChunks
          xyneigh = M.lookup (V2 (xi+1) (yi+1)) landscapeChunks
          mneighbours = (xneigh, yneigh, xyneigh)
      landMesh <- makeLandMesh (V2 landscapeChunkSize landscapeChunkSize) landscapeTileScale landscapeResolution landscapeVerticalScale mneighbours chunk
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
  , landscapeUpdatedChunks = S.singleton $ IntRect lowBoundX highBoundY highBoundX highBoundY
  }
  where
    (V2 lowBoundX lowBoundY, V2 highBoundX highBoundY) = landscapeBounds land
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
  , landscapeUpdatedChunks = S.singleton $ IntRect lowBoundX highBoundY highBoundX highBoundY
  }
  where
  (V2 lowBoundX lowBoundY, V2 highBoundX highBoundY) = landscapeBounds land
  v2 v = V2 v v
  chunkSize = v2 $ landscapeChunkSize land
  updChunk chunkCoord chunk = chunk { landChunkTiles = updTiles $ landChunkTiles chunk}
    where
    chunkOrigin = chunkCoord * chunkSize
    updTiles arr = runIdentity . R.computeP . R.traverse arr id $ \_ (R.Z R.:. y R.:. x) -> let
      R.Z R.:. height R.:. _ = R.extent arr
      in f $ chunkOrigin + V2 x y

-- | Set heights in landscape for region from function. Coordinates passed in the function are world coordinates and
-- outup height is in world units too.
landscapeUpdateHeights :: V2 Float -- ^ Offset of region in world units
  -> V2 Float -- ^ Size of region in world units
  -> (V2 Float -> Float -> Float)
  -> Landscape -> Landscape
landscapeUpdateHeights (V2 ox oy) (V2 sx sy) f land = land {
    landscapeChunks = M.mapWithKey updChunk $ landscapeChunks land
  , landscapeUpdatedChunks = S.insert updRect $ landscapeUpdatedChunks land
  }
  where
    updRect = IntRect (floor ox) (floor oy) (floor ox + ceiling sx - 1) (floor oy + ceiling sy - 1)
    chsize = landscapeChunkSize land
    ox' = floor ox `div` chsize
    oy' = floor oy `div` chsize
    sx' = ((floor ox + (abs . ceiling) sx) `div` chsize) - ox' + 1
    sy' = ((floor oy + (abs . ceiling) sy) `div` chsize) - oy' + 1
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
            in if    chunkPoint ^. _x >= ox && chunkPoint ^. _x < ox+sx
                  && chunkPoint ^. _y >= oy && chunkPoint ^. _y < oy+sy
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
  , landscapeUpdatedChunks = S.insert (IntRect ox oy (ox+sx-1) (oy+sy-1)) $ landscapeUpdatedChunks land
  }
  where
    chsize = landscapeChunkSize land
    ox' = ox `div` chsize
    oy' = oy `div` chsize
    sx' = ((ox + abs sx) `div` chsize) - ox' + 1
    sy' = ((oy + abs sy) `div` chsize) - oy' + 1
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
  -> IntRect -- ^ Region of chunk to update
  -> IO ()
landscapeUpdateChunk LoadedLandscape{..} pos region = do
  let land = loadedLandDatum
      mchunk = M.lookup pos $ landscapeChunks land
      mmesh = M.lookup pos loadedLandChunks
      mx  = M.lookup (pos + V2 1 0) $ landscapeChunks land
      my  = M.lookup (pos + V2 0 1) $ landscapeChunks land
      mxy = M.lookup (pos + V2 1 1) $ landscapeChunks land
  case (mchunk, mmesh) of
    (Just chunk, Just mesh) -> updateChunkMesh chunk (mx, my, mxy) mesh region
    _ -> pure ()

-- | Update loaded landscape and sync changes with engine resources
updateLoadedLandscape :: (Landscape -> Landscape) -> LoadedLandscape -> IO LoadedLandscape
updateLoadedLandscape f ll= do
  let land = f $ loadedLandDatum ll
      ll' = ll { loadedLandDatum = land }
      splitter :: IntRect -> [(V2 Int, IntRect)]
      splitter = regionChunks $ fromIntegral $ landscapeChunkSize land
      regions :: [(V2 Int, IntRect)]
      regions = mconcat $ fmap splitter $ S.elems $ landscapeUpdatedChunks land
  traverse_ (uncurry $ landscapeUpdateChunk ll') regions
  pure ll' {
      loadedLandDatum = (loadedLandDatum ll') { landscapeUpdatedChunks = mempty }
    }

-- | Convert region to set of subregions splitted by chunks borders
--
-- Also adds minus direction chunks borders to avoid gaps on terrain update.
regionChunks :: V2 Int -> IntRect -> [(V2 Int, IntRect)]
regionChunks (V2 chunkWidth chunkHeight) (IntRect minx miny maxx maxy) = [(V2 x y, IntRect minx' miny' maxx' maxy')
  | (x, minx', maxx') <- split minx maxx chunkWidth
  , (y, miny', maxy') <- split miny maxy chunkHeight
  ]
  where
    split :: Int -> Int -> Int -> [(Int, Int, Int)]
    split minv maxv dv = gaphack ++ if minv `div` dv == maxv `div` dv
      then [(minv `div` dv, minv `mod` dv, maxv `mod` dv)]
      else startv : [(v, 0, dv-1) | v <- [minv `div` dv + 1 .. maxv `div` dv - 1] ] ++ [endv]
      where
        startv = (minv `div` dv, minv `mod` dv, dv-1)
        endv = (maxv `div` dv, 0, maxv `mod` dv)
        gaphack =
             [(minv `div` dv - 1, dv-1, dv-1)| minv `mod` dv == 0]
          ++ [(maxv `div` dv + 1, 0, 0)| (maxv+1) `mod` dv == 0]

-- | Set heights in landscape for region from function. Coordinates passed in the function are world coordinates and
-- outup height is in world units too.
landscapeAddCircleHeights :: V2 Float -- ^ Center of circle (world units)
  -> V2 Float -- ^ Size of circle (radius) world units
  -> Float -- ^ Height in center
  -> Landscape -> Landscape
landscapeAddCircleHeights center size@(V2 sx sy) h l = landscapeUpdateHeights corner (2 * tileSize) f l
  where
    tsize = landscapeTileScale l
    tileSize = size / V2 tsize tsize
    corner = center - size
    f p v = let
      V2 x y = p - center
      e2  = 1 - x^2 / sx^2 - y^2 / sy^2
      v' = if e2 > 0 then h * sqrt e2 else 0
      in v + v'

-- | Get current tile in landscape
landscapeGetTile :: Landscape -- ^ land data
  -> V2 Int -- ^ Position
  -> Word8 -- ^ TileId
landscapeGetTile land (V2 px py) = case M.lookup (V2 chx chy) $ landscapeChunks land of
  Nothing -> 0
  Just chunk -> R.index (landChunkTiles chunk) (R.ix2 y' x')
  where
    chunkSize = landscapeChunkSize land
    chx = px `div` chunkSize
    chy = py `div` chunkSize
    x' = px `mod` chunkSize
    y' = py `mod` chunkSize

-- | Get current tile in landscape
landscapeGetTileVariance :: Landscape -- ^ land data
  -> V2 Int -- ^ Position
  -> Word8 -- ^ Variance id
landscapeGetTileVariance land (V2 px py) = case M.lookup (V2 chx chy) $ landscapeChunks land of
  Nothing -> 0
  Just chunk -> R.index (landChunkTilesVar chunk) (R.ix2 y' x')
  where
    chunkSize = landscapeChunkSize land
    chx = px `div` chunkSize
    chy = py `div` chunkSize
    x' = px `mod` chunkSize
    y' = py `mod` chunkSize

-- | Get current tile in landscape
landscapeGetHeight :: Landscape -- ^ land data
  -> V2 Float -- ^ Position
  -> Float -- ^ Height value
landscapeGetHeight land (V2 px py) = case M.lookup (V2 chx chy) $ landscapeChunks land of
  Nothing -> 0
  Just chunk -> R.index (landChunkHeightmap chunk) (R.ix2 y' x')
    where
      R.Z R.:. hh R.:. hw = R.extent $ landChunkHeightmap chunk
      x' = floor $ fromIntegral hw * px / fromIntegral chunkSize
      y' = floor $ fromIntegral hh * py / fromIntegral chunkSize
  where
    chunkSize = landscapeChunkSize land
    chx = floor px `div` chunkSize
    chy = floor py `div` chunkSize
