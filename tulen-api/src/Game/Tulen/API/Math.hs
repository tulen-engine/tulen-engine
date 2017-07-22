module Game.Tulen.API.Math(
    V2(..)
  , V3(..)
  , V4(..)
  , Rect(..)
  , Normal
  ) where

import GHC.Generics

-- | Two dimentional vector with elements of type 'a'
data V2 a = V2 !a !a
  deriving (Eq, Ord, Show, Read, Functor, Generic)

-- | Two dimentional vector with elements of type 'a'
data V3 a = V3 !a !a !a
  deriving (Eq, Ord, Show, Read, Functor, Generic)

-- | Two dimentional vector with elements of type 'a'
data V4 a = V4 !a !a !a !a
  deriving (Eq, Ord, Show, Read, Functor, Generic)

-- | Normal vector shortcut
type Normal = V3 Double

-- | Rectangular area with coodinates of type 'a'
data Rect a = Rect {
  rectMinX :: !a
, rectMinY :: !a
, rectMaxX :: !a
, rectMaxY :: !a
} deriving (Eq, Ord, Show, Read, Functor, Generic)
