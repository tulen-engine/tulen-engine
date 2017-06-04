module Game.Tulen.Core(
    Core
  ) where

import Graphics.Urho3D

data Core = Core {
  coreApplication :: SharedPtr Application
}
