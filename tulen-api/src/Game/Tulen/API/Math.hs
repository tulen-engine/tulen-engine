module Game.Tulen.API.Math(
    V2(..)
  , V3(..)
  , V4(..)
  ) where

import GHC.Generics

-- | Two dimentional vector with elements of type 'a'
data V2 a = V2 {-# UNPACK #-} !a {-# UNPACK #-} !a
  deriving (Eq, Ord, Show, Functor, Read, Generic)

-- | Two dimentional vector with elements of type 'a'
data V3 a = V3 {-# UNPACK #-} !a {-# UNPACK #-} !a {-# UNPACK #-} !a
  deriving (Eq, Ord, Show, Functor, Read, Generic)

-- | Two dimentional vector with elements of type 'a'
data V4 a = V4 {-# UNPACK #-} !a {-# UNPACK #-} !a {-# UNPACK #-} !a {-# UNPACK #-} !a
  deriving (Eq, Ord, Show, Functor, Read, Generic)
