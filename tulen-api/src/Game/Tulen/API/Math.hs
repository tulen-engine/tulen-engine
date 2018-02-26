module Game.Tulen.API.Math(
    V2(..)
  , V3(..)
  , V4(..)
  , Rect(..)
  , Normal
  , ViewPoint
  , Color(..)
  , Word8
  ) where

import Data.Word
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

-- | Point in viewport. Origin is top-left corner and X axis goes right, Y axis
-- goes down. Units are pixels.
type ViewPoint = V2 Int

-- | Rectangular area with coodinates of type 'a'
data Rect a = Rect {
  rectMinX :: !a
, rectMinY :: !a
, rectMaxX :: !a
, rectMaxY :: !a
} deriving (Eq, Ord, Show, Read, Functor, Generic)

-- | RGBA8 formated color
data Color = Color {
  colorRed   :: !Word8 -- ^ Red component 0-255
, colorGreen :: !Word8 -- ^ Green component 0-255
, colorBlue  :: !Word8 -- ^ Blue component 0-255
, colorAlpha :: !Word8 -- ^ Alpha component 0-255
} deriving (Eq, Ord, Show, Read, Generic)
