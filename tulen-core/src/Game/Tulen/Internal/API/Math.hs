module Game.Tulen.Internal.API.Math(

  ) where

import Game.Tulen.Internal.API.Helpers
import Game.Tulen.API.Math
import Graphics.Urho3D

instance ToAPI Vector2 (V2 Float) where
  toAPI (Vector2 xv yv) = V2 xv yv
  {-# INLINE toAPI #-}

instance ToAPI Vector3 (V3 Float) where
  toAPI (Vector3 xv yv zv) = V3 xv yv zv
  {-# INLINE toAPI #-}

instance ToAPI Vector4 (V4 Float) where
  toAPI (Vector4 xv yv zv wv) = V4 xv yv zv wv
  {-# INLINE toAPI #-}

instance FromAPI Vector2 (V2 Float) where
  fromAPI (V2 xv yv) = Vector2 xv yv
  {-# INLINE fromAPI #-}

instance FromAPI Vector3 (V3 Float) where
  fromAPI (V3 xv yv zv) = Vector3 xv yv zv
  {-# INLINE fromAPI #-}

instance FromAPI Vector4 (V4 Float) where
  fromAPI (V4 xv yv zv wv) = Vector4 xv yv zv wv
  {-# INLINE fromAPI #-}

instance ToAPI Vector2 (V2 Double) where
  toAPI (Vector2 xv yv) = V2 (realToFrac xv) (realToFrac yv)
  {-# INLINE toAPI #-}

instance ToAPI Vector3 (V3 Double) where
  toAPI (Vector3 xv yv zv) = V3 (realToFrac xv) (realToFrac yv) (realToFrac zv)
  {-# INLINE toAPI #-}

instance ToAPI Vector4 (V4 Double) where
  toAPI (Vector4 xv yv zv wv) = V4 (realToFrac xv) (realToFrac yv) (realToFrac zv) (realToFrac wv)
  {-# INLINE toAPI #-}

instance FromAPI Vector2 (V2 Double) where
  fromAPI (V2 xv yv) = Vector2 (realToFrac xv) (realToFrac yv)
  {-# INLINE fromAPI #-}

instance FromAPI Vector3 (V3 Double) where
  fromAPI (V3 xv yv zv) = Vector3 (realToFrac xv) (realToFrac yv) (realToFrac zv)
  {-# INLINE fromAPI #-}

instance FromAPI Vector4 (V4 Double) where
  fromAPI (V4 xv yv zv wv) = Vector4 (realToFrac xv) (realToFrac yv) (realToFrac zv) (realToFrac wv)
  {-# INLINE fromAPI #-}

instance ToAPI IntVector2 (V2 Int) where
  toAPI (IntVector2 xv yv) = V2 (fromIntegral xv) (fromIntegral yv)
  {-# INLINE toAPI #-}

instance FromAPI IntVector2 (V2 Int) where
  fromAPI (V2 xv yv) = IntVector2 (fromIntegral xv) (fromIntegral yv)
  {-# INLINE fromAPI #-}
