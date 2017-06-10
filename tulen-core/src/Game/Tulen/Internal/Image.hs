-- | Tools to converting repa arrays to and from urho image
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.Tulen.Internal.Image where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Array.Repa (Array, U, DIM2, Z(..), (:.)(..))
import Data.Vector.Unboxed.Deriving
import Graphics.Urho3D
import Linear

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

derivingUnbox "Color"
  [t| Color -> (Float, Float, Float, Float) |]
  [| \(Color r g b a) -> (r, g, b, a) |]
  [| \(r, g, b, a) -> Color r g b a   |]

-- | Haskell side image data
type RepaImage = Array U DIM2 Color

-- | Copy data from urho image to repa array
copyFromImage :: (Parent Image a, Pointer ptr a, MonadIO m, PrimMonad m)
  => ptr -- ^ Pointer to image
  -> m RepaImage
copyFromImage ptr = do
  w <- imageGetWidth ptr
  h <- imageGetHeight ptr
  let dims = Z :. h :. w
      arr = R.computeS $ R.fromFunction dims $ const $ Color 0 0 0 0
      is = [0 .. w * h - 1] :: [Int]
  marr <- VU.unsafeThaw $ R.toUnboxed arr
  forM_ is $ \i -> do
    let xy@(Z :. y :. x) = R.fromIndex dims i
    color <- imageGetPixel2D ptr x y
    VUM.write marr i color
  pure arr

-- | Copy data from repa array to urho image
copyToImage :: (Parent Image a, Pointer ptr a, MonadIO m, PrimMonad m)
  => ptr -- ^ Pointer to image
  -> RepaImage -- ^ Datum
  -> m ()
copyToImage ptr arr = do
  let
    dims@(Z :. height :. width) = R.extent arr
    writePixel i c = do
      let Z :. y :. x = R.fromIndex dims i
      imageSetPixel2D ptr x y c
  imageSetSize2D ptr width height 4
  VU.imapM_ writePixel $ R.toUnboxed arr

-- | Copy a region from one array to another
blitRegion :: Monad m
  => RepaImage -- ^ Source
  -> RepaImage -- ^ Destination
  -> V2 Int -- ^ Offset of source
  -> V2 Int -- ^ Offset of dist
  -> V2 Int -- ^ Size of region to blit
  -> m RepaImage -- ^ Result
blitRegion src dist (V2 ox oy) (V2 dx dy) (V2 sx sy) = R.computeP $ R.traverse2 src dist (\_ sh -> sh) $ \getSrc getDist xy@(Z :. y :. x) ->
  if x >= dx && y >= dy && x < dx+sx && y < dy+sy then getSrc (Z :. (y + oy - dy) :. (x + ox - dx) ) else getDist xy

-- | Copy a region from one array to another flipped vertical
blitRegionVFlip :: Monad m
  => RepaImage -- ^ Source
  -> RepaImage -- ^ Destination
  -> V2 Int -- ^ Offset of source
  -> V2 Int -- ^ Offset of dist
  -> V2 Int -- ^ Size of region to blit
  -> m RepaImage -- ^ Result
blitRegionVFlip src dist (V2 ox oy) (V2 dx dy) (V2 sx sy) = R.computeP $ R.traverse2 src dist (\_ sh -> sh) $ \getSrc getDist xy@(Z :. y :. x) ->
  if x >= dx && y >= dy && x < dx+sx && y < dy+sy then getSrc (Z :. (oy + (sy - 1 - y + dy)) :. (x + ox - dx) ) else getDist xy

-- | Copy a region from one array to another flipped horizontal
blitRegionHFlip :: Monad m
  => RepaImage -- ^ Source
  -> RepaImage -- ^ Destination
  -> V2 Int -- ^ Offset of source
  -> V2 Int -- ^ Offset of dist
  -> V2 Int -- ^ Size of region to blit
  -> m RepaImage -- ^ Result
blitRegionHFlip src dist (V2 ox oy) (V2 dx dy) (V2 sx sy) = R.computeP $ R.traverse2 src dist (\_ sh -> sh) $ \getSrc getDist xy@(Z :. y :. x) ->
  if x >= dx && y >= dy && x < dx+sx && y < dy+sy then getSrc (Z :. (y + oy - dy) :. (ox + (sx - 1 - x + dx)) ) else getDist xy

-- | Copy a region from one array to another flipped horizontal and vertical
blitRegionFlip :: Monad m
  => RepaImage -- ^ Source
  -> RepaImage -- ^ Destination
  -> V2 Int -- ^ Offset of source
  -> V2 Int -- ^ Offset of dist
  -> V2 Int -- ^ Size of region to blit
  -> m RepaImage -- ^ Result
blitRegionFlip src dist (V2 ox oy) (V2 dx dy) (V2 sx sy) = R.computeP $ R.traverse2 src dist (\_ sh -> sh) $ \getSrc getDist xy@(Z :. y :. x) ->
  if x >= dx && y >= dy && x < dx+sx && y < dy+sy then getSrc (Z :. (oy + (sy - 1 - y + dy)) :. (ox + (sx - 1 - x + dx)) ) else getDist xy

-- | Copy a region from one array to another with border that consists of mirrored source texture
blitRegionWithBorder :: Monad m
  => RepaImage -- ^ Source
  -> RepaImage -- ^ Destination
  -> V2 Int -- ^ Offset of source
  -> V2 Int -- ^ Offset of dist
  -> V2 Int -- ^ Size of region to blit
  -> V2 Int -- ^ Border size
  -> m RepaImage -- ^ Result
blitRegionWithBorder src dist sourceOffset distOffset size@(V2 sx sy) borderSize@(V2 bx by) = do
  -- blit central
  centerBlit <- blitRegion src dist sourceOffset (distOffset+borderSize) size
  -- top left corner
  topLeftCBlit <- blitRegionFlip src centerBlit sourceOffset distOffset borderSize
  -- top right corner
  topRightCBlit <- blitRegionFlip src topLeftCBlit (sourceOffset + V2 (sx - bx) 0 ) (distOffset + V2 (bx + sx) 0) borderSize
  -- bottom left corner
  bottomLeftCBlit <- blitRegionFlip src topRightCBlit (sourceOffset + V2 0 (sy - by)) (distOffset + V2 0 (by + sy)) borderSize
  -- bottom right corner
  bottomRightCBlit <- blitRegionFlip src bottomLeftCBlit (sourceOffset + size - borderSize) (distOffset + borderSize + size) borderSize
  -- top border
  topBorderBlit <- blitRegionVFlip src bottomRightCBlit sourceOffset (distOffset + V2 bx 0) (V2 sx by)
  -- bottom border
  bottomBorderBlit <- blitRegionVFlip src topBorderBlit (sourceOffset + V2 0 (sy - by)) (distOffset + V2 bx (by+sy)) (V2 sx by)
  -- left border
  leftBorderBlit <- blitRegionHFlip src bottomBorderBlit sourceOffset (distOffset + V2 0 by) (V2 bx sy)
  -- right border
  rightBorderBlit <- blitRegionHFlip src leftBorderBlit (sourceOffset + V2 (sx - bx) 0) (distOffset + V2 (bx + sx) by) (V2 bx sy)
  pure rightBorderBlit
