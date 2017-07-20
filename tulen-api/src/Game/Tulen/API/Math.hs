module Game.Tulen.API.Math(
    V2(..)
  , V3(..)
  , V4(..)
  , Normal
  ) where

import GHC.Generics

-- | Two dimentional vector with elements of type 'a'
data V2 a = V2 !a !a
  deriving (Eq, Ord, Show, Functor, Read, Generic)

-- | Two dimentional vector with elements of type 'a'
data V3 a = V3 !a !a !a
  deriving (Eq, Ord, Show, Functor, Read, Generic)

-- | Two dimentional vector with elements of type 'a'
data V4 a = V4 !a !a !a !a
  deriving (Eq, Ord, Show, Functor, Read, Generic)

-- | Normal vector shortcut
type Normal = V3 Double
