-- | Generation of textures for landscape
{-# LANGUAGE MultiWayIf #-}
module Game.Tulen.Internal.Landscape.Texture where

import Control.Monad.Reader
import Data.Array.Repa (Z(..), (:.)(..))
import Data.Word
import Foreign
import Game.Tulen.Internal.Core.Types
import Game.Tulen.Internal.Image
import Game.Tulen.Internal.Landscape.Types
import Graphics.Urho3D
import Linear

import qualified Data.Array.Repa as R
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

-- | Copy all tiles to image, encodes corners of quad in each of RGBA channels.
copyTilesToImage :: Tilemap -- ^ Original tilemap
  -> (Maybe Tilemap, Maybe Tilemap, Maybe Tilemap) -- ^ Optional neighbour tiles in (+X, +Y, +XY) directions
  -> IntRect -- ^ Region of tilemap to copy
  -> SharedPtr Image -- ^ Where to pack data
  -> IO ()
copyTilesToImage tm (mxm, mym, mxym) (IntRect minx miny maxx maxy) img = mapM_ writePixel is
  where
    R.Z R.:. height R.:. width = R.extent tm
    minx' = max 0 minx
    miny' = max 0 miny
    maxx' = min (width-1) maxx
    maxy' = min (height-1) maxy
    is :: [(Int, Int)]
    is = (,) <$> [miny' .. maxy'] <*> [minx' .. maxx']

    lookupTilemap :: Tilemap -> Int -> Int -> Word8
    lookupTilemap m x y = R.toUnboxed m UV.! R.toIndex (R.extent m) (R.Z R.:. y R.:. x)

    -- lookup with neigbours
    nlookup :: Int -> Int -> Word8
    nlookup x y
      | x >= width && y >= height, Just xym <- mxym = lookupTilemap xym 0 0
      | x >= width && y >= height, Nothing <- mxym = 0
      | x >= width, Just xm <- mxm = lookupTilemap xm 0 y
      | y >= height, Just ym <- mym = lookupTilemap ym x 0
      | x >= width || y >= height = 0
      | otherwise = lookupTilemap tm x y

    writePixel (y, x) = do
      let toFloat v = (fromIntegral v + 0.01) / 256
          c0 = toFloat $ lookupTilemap tm x y
          c1 = toFloat $ nlookup (x+1) y
          c2 = toFloat $ nlookup x (y+1)
          c3 = toFloat $ nlookup (x+1) (y+1)
      imageSetPixel2D img x (height - 1 - y) $ Color c0 c1 c2 c3

-- | Generate texture for tile mapping of chunk
makeDetailTexture :: (MonadIO m, MonadReader Core m)
  => (Maybe Tilemap, Maybe Tilemap, Maybe Tilemap) -- ^ Optional neighbour tiles in (+X, +Y, +XY) directions
  -> LandChunk
  -> m (SharedPtr Image, SharedPtr Texture2D)
makeDetailTexture mneighbours LandChunk{..} =  do
  context <- getContext =<< asks coreApplication
  -- create image
  img :: SharedPtr Image <- newSharedObject $ pointer context
  let R.Z R.:. height R.:. width = R.extent landChunkTiles
  imageSetSize2D img width height 4

  -- fill with data from tilemap
  liftIO $ copyTilesToImage landChunkTiles mneighbours (IntRect 0 0 width height) img

  -- create texture
  tex :: SharedPtr Texture2D <- newSharedObject $ pointer context
  textureSetFilterMode tex FilterNearest
  textureSetNumLevels tex 1
  _ <- texture2DSetSize tex width height getRGBAFormat TextureDynamic 1 True
  _ <- texture2DSetDataFromImage tex img False

  pure (img, tex)

-- | Generate texture for tile mapping of chunk
updateDetailTexture :: MonadIO m
  => (Maybe Tilemap, Maybe Tilemap, Maybe Tilemap) -- ^ Optional neighbour tiles in (+X, +Y, +XY) directions
  -> LandChunk -- ^ Chunk that contains new data
  -> IntRect -- ^ Region to update
  -> SharedPtr Image -- ^ Loaded image data
  -> SharedPtr Texture2D -- ^ Where to store final image data
  -> m ()
updateDetailTexture mneighbours LandChunk{..} (IntRect minx miny maxx maxy) img tex = do
  let R.Z R.:. height R.:. width = R.extent landChunkTiles
  imageSetSize2D img width height 4
  let minx' = max 0 (minx-1)
      miny' = max 0 (miny-1)
      maxx' = min (width-1) maxx
      maxy' = min (height-1) maxy
  liftIO $ copyTilesToImage landChunkTiles mneighbours (IntRect minx' miny' maxx' maxy') img
  texture2DSetDataFromImage tex img False
  pure ()

-- | How much pixels to add into atlas to prevent edge bleeding
atlasBleedingBorder :: Int
atlasBleedingBorder = 0

-- | Size of tile in pixels in atlas
atlasTileSize :: Int
atlasTileSize = 64

-- | Size of bordered tile in pixels in atlas
atlasBorderedTileSize :: Int
atlasBorderedTileSize = atlasTileSize + 2 * atlasBleedingBorder

-- | Fixed size of texture atlas in tiles
atlasSize :: V2 Int
atlasSize = V2 8 4

-- | Fixed size of texture atlas in pixels
atlasSizePixels :: V2 Int
atlasSizePixels = (* atlasTileSize) <$> atlasSize

-- | Transform position in tiles to position in pixels for original atlas
toTilePosition :: V2 Int -> V2 Int
toTilePosition = fmap (* atlasTileSize)

-- | Transform position in tiles to position in pixels for bordered atlas
toBorderedTilePosition :: V2 Int -> V2 Int
toBorderedTilePosition = fmap (* atlasBorderedTileSize)

-- | Fixed size of texture atlas with bleeding border
atlasSizePixelsWithBorder :: V2 Int
atlasSizePixelsWithBorder = toBorderedTilePosition atlasSize

-- | Check if the tileset doesn't have additional random tiles
isSmallTileset :: Ptr Image -> IO Bool
isSmallTileset ptr = do
  w <- imageGetWidth ptr
  pure $ w < 8 * atlasTileSize

-- | Fill random tiles sector with default tile
extendTileset :: Monad m => RepaImage -> RepaImage -> m RepaImage
extendTileset src dist = F.foldlM blit dist is
  where
    size = 4
    offset = 4
    is = [0 .. size * size - 1] :: [Int]
    unindex = R.fromIndex (Z :. size :. size)

    blit img i = do
      let Z :. yi :. xi = unindex i
          sourcePos = V2 0 0
          distPos = toBorderedTilePosition $ V2 (xi + 4) yi
      blitRegionWithBorder src img sourcePos distPos (V2 atlasTileSize atlasTileSize) (V2 atlasBleedingBorder atlasBleedingBorder)

-- | Copy given tiles to destination with border
copyTilesWithBorder :: Monad m => RepaImage -> RepaImage -> [V2 Int] -> m RepaImage
copyTilesWithBorder src = F.foldlM blit
  where
    blit img vi = blitRegionWithBorder src img (toTilePosition vi) (toBorderedTilePosition vi)
      (V2 atlasTileSize atlasTileSize) (V2 atlasBleedingBorder atlasBleedingBorder)

-- | Generate texture array for tilesets
makeTilesTexture :: (MonadIO m, MonadReader Core m)
  => V.Vector TileInfo
  -> m (SharedPtr Texture2DArray)
makeTilesTexture tileInfos = do
  context <- getContext =<< asks coreApplication
  cache <- asks coreResourceCache
  tex :: SharedPtr Texture2DArray <- newSharedObject context
  let layersCount = fromIntegral $ length tileInfos
      V2 texWidth texHeight = atlasSizePixelsWithBorder
  texture2DArraySetSize tex layersCount texWidth texHeight getRGBAFormat TextureStatic
  textureSetFilterMode tex FilterNearestAnisotropic
  textureSetAddressMode tex CoordU AddressClamp
  textureSetAddressMode tex CoordV AddressClamp
  liftIO $ mapM_ (loadLayer cache tex) $ V.indexed tileInfos
  pure tex
  where
    loadLayer cache tex (i, TileInfo{..}) = do
      mtex <- cacheGetResource cache tileResource True
      case mtex of
        Nothing -> fail $ "Failed to load tileset texture " ++ tileResource -- TODO: proper error handling
        Just img -> do
          putStrLn $ "Generating atlas for " ++ show tileResource
          isSmall <- isSmallTileset img
          when (atlasBleedingBorder /= 0 || isSmall) $ do -- no need to modify when no border is defined
            srcArr <- copyFromImage img
            let V2 sx sy = atlasSizePixelsWithBorder
                distArr = R.computeS $ R.fromFunction (Z :. sy :. sx) (const $ Color 0 0 0 0)
            distBlit <- if isSmall
              then do
                arr <- extendTileset srcArr distArr
                copyTilesWithBorder srcArr arr [V2 x y | x <- [0 .. 3], y <- [0 .. 3]]
              else copyTilesWithBorder srcArr distArr [V2 x y | x <- [0 .. 7], y <- [0 .. 3]]
            copyToImage img distBlit
          -- imageSavePNG img $ "test" ++ show i ++ ".png"
          res <- texture2DArraySetDataFromImage tex (fromIntegral i) (pointer img) False
          unless res $ putStrLn $ "Failed to bind tileset texture " ++ tileResource ++ " to texture array"

-- | Render technique for landscape
landscapeTechniqueName :: String
landscapeTechniqueName = "Techniques/LandscapeBlend.xml"

-- | Create landscape material that can be cloned between different chunks.
makeLandscapeMaterial :: (MonadIO m, MonadReader Core m) => m (SharedPtr Material)
makeLandscapeMaterial = do
  context <- getContext =<< asks coreApplication
  cache <- asks coreResourceCache
  mtech <- cacheGetResource cache landscapeTechniqueName True
  case mtech of
    Nothing -> fail $ "Failed to load technique for landscape " ++ landscapeTechniqueName -- TODO: error handling
    Just tech -> do
      mat :: SharedPtr Material <- newSharedObject context
      materialSetNumTechniques mat 1
      materialSetTechnique mat 0 tech 0 0.0
      materialSetShaderParameter mat "MatSpecColor" $ Color 0 0 0 16
      materialSetShaderParameter mat "ChunkSize" (10 :: Int)
      pure mat
