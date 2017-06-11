-- | Generation of textures for landscape
module Game.Tulen.Internal.Landscape.Texture where

import Data.Word
import Foreign
import Game.Tulen.Internal.Image
import Game.Tulen.Internal.Landscape.Types
import Graphics.Urho3D
import Linear
import Data.Array.Repa (Z(..), (:.)(..))

import qualified Data.Array.Repa as R
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

-- | Copy all tiles to image
-- TODO: neighbour lookup
copyTilesToImage :: Tilemap -> SharedPtr Image -> IO ()
copyTilesToImage tm img = mapM_ writePixel is
  where
    R.Z R.:. height R.:. width = R.extent tm
    is :: [(Int, Int)]
    is = (,) <$> [0 .. height - 1] <*> [0 .. width - 1]

    lookupTilemap :: Int -> Int -> Word8
    lookupTilemap x y = R.toUnboxed tm UV.! R.toIndex (R.extent tm) (R.Z R.:. y R.:. x)

    writePixel (x, y) = do
      let toFloat v = (fromIntegral v + 0.01) / 256
          c0 = toFloat $ lookupTilemap x y
          c1 = if x == width - 1 then 0
            else toFloat $ lookupTilemap (x+1) y
          c2 = if y == height - 1 then 0
            else toFloat $ lookupTilemap x (y+1)
          c3 = if x == width - 1 || y == height - 1 then 0
            else toFloat $ lookupTilemap (x+1) (y+1)
      imageSetPixel2D img x (height - 1 - y) $ Color c0 c1 c2 c3

-- | Generate texture for tile mapping of chunk
makeDetailTexture :: Ptr Context
  -> LandChunk
  -> IO (SharedPtr Texture2D)
makeDetailTexture context LandChunk{..} =  do
  -- create image
  img :: SharedPtr Image <- newSharedObject $ pointer context
  let R.Z R.:. height R.:. width = R.extent landChunkTiles
  imageSetSize2D img width height 4

  -- fill with data from tilemap
  copyTilesToImage landChunkTiles img -- TODO: here (problem with out of sync size of tile texture in shader!)

  -- create texture
  tex :: SharedPtr Texture2D <- newSharedObject $ pointer context
  textureSetFilterMode tex FilterNearest
  textureSetNumLevels tex 1
  _ <- texture2DSetSize tex width height getRGBAFormat TextureDynamic 1 True
  _ <- texture2DSetDataFromImage tex img False

  pure tex

-- | How much pixels to add into atlas to prevent edge bleeding
atlasBleedingBorder :: Int
atlasBleedingBorder = 10

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
makeTilesTexture :: SharedPtr Application
  -> V.Vector TileInfo
  -> IO (SharedPtr Texture2DArray)
makeTilesTexture app tileInfos = do
  context <- getContext app
  tex :: SharedPtr Texture2DArray <- newSharedObject context
  let layersCount = fromIntegral $ length tileInfos
      V2 texWidth texHeight = atlasSizePixelsWithBorder
  texture2DArraySetSize tex layersCount texWidth texHeight getRGBAFormat TextureStatic
  textureSetFilterMode tex FilterNearest
  textureSetAddressMode tex CoordU AddressMirror
  textureSetAddressMode tex CoordV AddressMirror
  textureSetNumLevels tex 1
  mapM_ (loadLayer tex) $ V.indexed tileInfos
  pure tex
  where
    loadLayer tex (i, TileInfo{..}) = do
      Just (cache :: Ptr ResourceCache) <- getSubsystem app
      mtex <- cacheGetResource cache tileResource True
      case mtex of
        Nothing -> fail $ "Failed to load tileset texture " ++ tileResource -- TODO: proper error handling
        Just img -> do
          srcArr <- copyFromImage img
          let V2 sx sy = atlasSizePixelsWithBorder
              distArr = R.computeS $ R.fromFunction (Z :. sy :. sx) (const $ Color 0 0 0 0)
          isSmall <- isSmallTileset img
          distBlit <- if isSmall
            then do
              arr <- extendTileset srcArr distArr
              copyTilesWithBorder srcArr arr [V2 x y | x <- [0 .. 3], y <- [0 .. 3]]
            else copyTilesWithBorder srcArr distArr [V2 x y | x <- [0 .. 7], y <- [0 .. 3]]
          copyToImage img distBlit
          -- imageSavePNG img $ "test" ++ show i ++ ".png"
          res <- texture2DArraySetDataFromImage tex (fromIntegral i) (pointer img) False
          unless res $ putStrLn $ "Failed to bind tileset texture " ++ tileResource ++ " to texture array"
