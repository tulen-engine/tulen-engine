-- | All-in module that reexports API of core.
module Game.Tulen.Core(
  -- | Root reference
    Core
  , CoreConfig(..)
  , defaultCoreConfig
  , runCore
  , TulenM
  ) where

import Game.Tulen.Internal.API
import Game.Tulen.Internal.Core
import Game.Tulen.Internal.Core.Types
import Game.Tulen.Internal.Monad 
