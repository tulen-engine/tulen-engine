module Game.Tulen.API.Mouse(
    ViewPoint
  , MouseButton(..)
  , MouseMonad(..)
  ) where

import Game.Tulen.API.Camera
import Game.Tulen.API.Math
import GHC.Generics
import Reflex

-- | Possible mouse buttons that you can track
data MouseButton = LeftButton | RightButton | MiddleButton | ExtraButton1 | ExtraButton2
  deriving (Generic, Eq, Ord, Enum, Bounded, Show, Read)

-- | Operations with mouse input
class Monad m => MouseMonad t m | m -> t where
  -- | Notify when given mouse button is clicked, return window space point.
  onMouseClick :: MouseButton -> m (Event t ViewPoint)
