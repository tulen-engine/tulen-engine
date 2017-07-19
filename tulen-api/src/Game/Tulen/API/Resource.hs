module Game.Tulen.API.Resource(
    ResourceRef(..)
  ) where

import GHC.Generics

-- | Path to resource file
newtype ResourceRef = ResourceRef { unResourceRef :: String }
  deriving (Eq, Ord, Show, Read, Generic)
