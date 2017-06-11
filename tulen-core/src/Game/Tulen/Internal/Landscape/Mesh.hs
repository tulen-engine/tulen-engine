-- | Mesh generation for landscape
module Game.Tulen.Internal.Landscape.Mesh where

import Data.Word
import Foreign
import Game.Tulen.Internal.Landscape.Types
import Game.Tulen.Internal.Landscape.Texture
import Graphics.Urho3D
import Linear

import qualified Data.Array.Repa as R
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

import Debug.Trace

-- | Holds all data for landscape mesh. Should be regenerated (or updated)
-- when original data chanes.
data LandMesh = LandMesh {
  -- | Engine model
  landMeshModel    :: !(SharedPtr Model)
  -- | Buffer that holds vertecies
, landMeshVertex   :: !(SharedPtr VertexBuffer)
  -- | Index buffer that holds triangles
, landMeshIndex    :: !(SharedPtr IndexBuffer)
  -- | Accumulated geometry
, landMeshGeometry :: !(SharedPtr Geometry)
  -- | Texture with data, where which tile is located (rgba channels each
  -- for corners of a quad).
, landMeshDetails  :: !(SharedPtr Texture2D)
}

-- | Strict pair for vertex and normal and UV
data VertWithNorm = VertWithNorm !(V3 Float) !(V3 Float) !(V2 Float)
  deriving (Show)

-- | TODO: ensure that alignment is honored
instance Storable VertWithNorm where
  sizeOf _  = 2 * sizeOf (undefined :: V3 Float) + sizeOf (undefined :: V2 Float)
  alignment _ = 1
  peek ptr = do
    v1 <- peek . castPtr $ ptr
    v2 <- peek . castPtr $ ptr `plusPtr` sizeOf (undefined :: V3 Float)
    v3 <- peek . castPtr $ ptr `plusPtr` (2 * sizeOf (undefined :: V3 Float))
    pure $ VertWithNorm v1 v2 v3
  poke ptr (VertWithNorm v1 v2 v3) = do
    poke (castPtr   ptr) v1
    poke (castPtr $ ptr `plusPtr` sizeOf (undefined :: V3 Float)) v2
    poke (castPtr $ ptr `plusPtr` (2 * sizeOf (undefined :: V3 Float))) v3

-- | Linear interpolation of function with two points
flerp :: Fractional a
  => a -- ^ x where we should calculate f(x)
  -> a -- ^ x1, first known point
  -> a -- ^ x2, second known point
  -> a -- ^ y1 = f(x1)
  -> a -- ^ y2 = f(x2)
  -> a -- ^ f(x)
flerp x x1 x2 y1 y2 = (x2 - x) / (x2 - x1) * y1 + (x - x1) / (x2 - x1) * y2

-- | Bilinear interpolation for estimation f(x, y) by four known points
biflerp :: Fractional a
  => a -- ^ x where we should calculate f(x, y)
  -> a -- ^ y where we should calculate f(x, y)
  -> a -- ^ x1 first known point
  -> a -- ^ x2 second known point
  -> a -- ^ y1 first known point
  -> a -- ^ y2 second known point
  -> a -- ^ p11 = f(x1, y1)
  -> a -- ^ p12 = f(x1, y2)
  -> a -- ^ p21 = f(x2, y1)
  -> a -- ^ p22 = f(x2, y2)
  -> a -- ^ f(x, y)
biflerp x y x1 x2 y1 y2 p11 p12 p21 p22 = flerp y y1 y2 fxy1 fxy2
  where
    fxy1 = flerp x x1 x2 p11 p21
    fxy2 = flerp x x1 x2 p12 p22

-- | Generate vertex-normal array for landscape chunk based on heightmap
genHeightVertecies :: V2 Int -- ^ Size in tiles
  -> Float -- ^ Size of one tile
  -> Int -- ^ Resolution (number of vertecies by tile)
  -> Float -- ^ Vertical scale of heightmap
  -> Heightmap -- ^ Heightmap of chunk
  -> SV.Vector VertWithNorm
genHeightVertecies (V2 sx sy) tsize res vsize hm = SV.fromList points
  where
    -- convert to model space from tile integral coords
    toModel :: Float -> Float -> (Float, Float)
    toModel x y = ( x * tsize, y * tsize )

    -- convert to heighmap pixel space from tile space coords
    toHeightmap :: Float -> Float -> (Float, Float)
    toHeightmap x y = let
      R.Z R.:. yhs R.:. xhs = R.extent hm
      in ( x / fromIntegral sx * fromIntegral xhs
         , (1 - y / fromIntegral sy) * fromIntegral yhs) -- Y axis inverted

    -- Input should be in [0 .. size] range, heightmap pixel space
    getHVal :: Float -> Float -> Float
    getHVal x y = let
      R.Z R.:. yhs R.:. xhs = R.extent hm
      xclamp = max 0 . min (xhs-1) . floor
      yclamp = max 0 . min (yhs-1) . floor
      in R.index hm (R.Z R.:. yclamp y R.:. xclamp x)

    -- Input should be in [0 .. size] range in hieghtmap pixel space
    getHeightHM :: Float -> Float -> Float
    getHeightHM x y = vsize * h -- bilinear interpolation
      where
      x1, x2, y1, y2 :: Float
      (x1, y1) = let f = fromIntegral . floor in (f x, f y)
      (x2, y2) = (x1 + 1, y1 + 1)
      p11 = getHVal x1 y1
      p12 = getHVal x1 y2
      p21 = getHVal x2 y1
      p22 = getHVal x2 y2
      h = biflerp x y x1 x2 y1 y2 p11 p12 p21 p22

    -- Input should be in [0 .. size] range in tile space
    getHeight :: Float -> Float -> Float
    getHeight xt yt = uncurry getHeightHM $ toHeightmap xt yt

    -- Input should be in [0 .. size-1] range in tile space
    getNormal :: Float -> Float -> V3 Float
    getNormal xt yt = normalize n
      where
      -- Current point
      (x, y) = toHeightmap xt yt
      z = getHeightHM x y
      p = V3 x z y
      -- First point for cross product
      x1 = x
      y1 = y+1 --if floor y == yhs-1 then y-1 else y+1
      z1 = getHeightHM x1 y1
      p1 = V3 x1 z1 y1
      -- Second point for cross product
      x2 = x+1 --if floor x == xhs-1 then x-1 else x+1
      y2 = y
      z2 = getHeightHM x2 y2
      p2 = V3 x2 z2 y2
      -- Vectors for cross product
      v1 = p1 - p
      v2 = p2 - p
      -- Cross product for normal
      n = v1 `cross` v2

    -- | Generate tiles numbers for generation of vertecies
    indecies :: [(Float, Float)]
    indecies = fmap divideByRes $ (,) <$> [0 .. sy * res - 1] <*> [0 .. sx * res - 1]
      where
        divideByRes (x, y) = (fromIntegral x / res', fromIntegral y / res')
        res' = fromIntegral res

    -- List comprehension of vertecies with normals
    points :: [VertWithNorm]
    points = concat $ flip fmap indecies $ \(y, x) ->let
      u0 = x
      u1 = u0 + dv
      t0 = y
      t1 = t0 + dv
      in [
          mkVertNorm  x        y       (V2 u0 t0)
        , mkVertNorm (x + dv)  y       (V2 u1 t0)
        , mkVertNorm  x       (y + dv) (V2 u0 t1)
        , mkVertNorm (x + dv) (y + dv) (V2 u1 t1)
        ]
      where
      dv = 1 / fromIntegral res
      mkVertNorm x y uv = let
        (vx, vy) = toModel x y
        vert = V3 vx (getHeight x y) vy
        norm = getNormal x y
        in VertWithNorm vert norm uv

-- | Helper that calculates bounding box of generated mesh.
--
-- Warning: expoits the fact that x and y coordinates are ascending.
calcBoundingBox :: SV.Vector VertWithNorm -> BoundingBox
calcBoundingBox vs = if SV.null vs then BoundingBox 0 0
  else BoundingBox (Vector3 minx minh miny) (Vector3 maxx maxh maxy)
  where
    inf = 1 / 0
    (minh, maxh) = SV.foldl' accMaxH (negate inf, inf) vs
    accMaxH (!minv, !maxv) (VertWithNorm (V3 _ h _) _ _) = let
      minv' = if minv < h then h else minv
      maxv' = if maxv > h then h else maxv
      in (minv', maxv')
    VertWithNorm (V3 maxx _ maxy) _ _ = SV.last vs
    VertWithNorm (V3 minx _ miny) _ _ = SV.head vs

-- | Generate triangle indecies for vertecies generated with 'genHeightVertecies'
genTriangleIndecies :: V2 Int -- ^ Size in tiles
  -> Float -- ^ Size of one tile
  -> Int -- ^ Resolution (number of vertecies by tile)
  -> Float -- ^ Vertical scale of heightmap
  -> Heightmap -- ^ Heightmap of chunk
  -> SV.Vector Word32
genTriangleIndecies (V2 sx sy) tsize res vsize hm = SV.fromList points
  where
    -- | Generate tiles numbers for generation of vertecies
    indecies :: [Word32]
    indecies = [0 .. fromIntegral (sy * sx * res * res) - 1]

    points :: [Word32]
    points = concat . flip fmap indecies $ \i -> let
      -- first triangle (counter clockwise order)
      i1 = i*4
      i2 = i1 + 2
      i3 = i1 + 3
      -- second triangle
      i4 = i1
      i5 = i1 + 3
      i6 = i1 + 1
      in [i1, i2, i3, i4, i5, i6]

-- | Generate new land mesh from given chunk description
makeLandMesh :: Ptr Context -- ^ Urho context
  -> V2 Int -- ^ Size in tiles of chunk
  -> Float -- ^ Size of single tile
  -> Int -- ^ Resolution (count of vertecies per tile)
  -> Float -- ^ Vertical scale of heightmap
  -> Maybe (Tilemap, Tilemap, Tilemap) -- ^ Optional neighbour tiles in (+X, +Y, +XY) directions
  -> LandChunk -- ^ chunk
  -> IO LandMesh
makeLandMesh context chunkSize tsize res vscale mneighbours ch@LandChunk{..} = do
  let vertNorms :: SV.Vector VertWithNorm = genHeightVertecies chunkSize tsize res vscale landChunkHeightmap
      numVertices = fromIntegral $ SV.length vertNorms
      indexData :: SV.Vector Word32 = genTriangleIndecies chunkSize tsize res vscale landChunkHeightmap
      numIndecies = fromIntegral $ SV.length indexData

  model :: SharedPtr Model <- newSharedObject $ pointer context
  vb :: SharedPtr VertexBuffer <- newSharedObject $ pointer context
  ib :: SharedPtr IndexBuffer <- newSharedObject $ pointer context
  geom :: SharedPtr Geometry  <- newSharedObject $ pointer context

  -- Shadowed buffer needed for raycasts to work, and so that data can be automatically restored on device loss
  vertexBufferSetShadowed vb True
  -- Fill vertex buffer
  let elements = [ vertexElement Type'Vector3 SEM'Position
                 , vertexElement Type'Vector3 SEM'Normal
                 , vertexElement Type'Vector2 SEM'TexCoord ]
  vertexBufferSetSize vb numVertices elements False
  _ <- SV.unsafeWith vertNorms $ vertexBufferSetData vb . castPtr

  indexBufferSetShadowed ib True
  indexBufferSetSize ib numIndecies True False
  _ <- SV.unsafeWith indexData $ indexBufferSetData ib . castPtr

  geometrySetVertexBuffer geom 0 (pointer vb)
  geometrySetIndexBuffer geom (pointer ib)
  geometrySetDrawRange geom TriangleList 0 numIndecies True

  _ <- modelSetNumGeometries model 1
  _ <- modelSetGeometry model 0 0 (pointer geom)
  modelSetBoundingBox model $ calcBoundingBox vertNorms

  -- Though not necessary to render, the vertex & index buffers must be listed in the model so that it can be saved properly
  let vertexBuffers = [vb]
      indexBuffers = [ib]
  -- Morph ranges could also be not defined. Here we simply define a zero range (no morphing) for the vertex buffer
      morphRangeStarts = [0]
      morphRangeCounts = [0]
  modelSetVertexBuffers model vertexBuffers morphRangeStarts morphRangeCounts
  modelSetIndexBuffers model indexBuffers

  tex <- makeDetailTexture context mneighbours ch
  pure LandMesh {
      landMeshModel    = model
    , landMeshVertex   = vb
    , landMeshIndex    = ib
    , landMeshGeometry = geom
    , landMeshDetails  = tex
    }
