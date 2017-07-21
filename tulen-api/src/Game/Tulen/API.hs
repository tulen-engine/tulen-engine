module Game.Tulen.API(
    TulenMonad
  , module Game.Tulen.API.Camera
  , module Game.Tulen.API.Landscape
  , module Game.Tulen.API.Math
  , module Game.Tulen.API.Mouse
  , module Game.Tulen.API.Resource
  -- * Reexports of control primitives
  , Reflex(..)
  , MonadAppHost
  , AppInfo
  , AppHost
  , HostFrame
  , performEventAndTrigger_
  , performEvent_
  , performEvent
  , switchAppHost
  , performAppHost
  , dynAppHost
  , holdAppHost
  , holdKeyAppHost
  , getPostBuild
  , performPostBuild
  , fmapMaybe
  , ffilter
  , ffor
  , fforMaybe
  , traceEvent
  , traceEventWith
  , gate
  , tag
  , tagPromptlyDyn
  , attach
  , attachWith
  , attachWithMaybe
  , attachPromptlyDyn
  , attachPromptlyDynWith
  , attachPromptlyDynWithMaybe
  , Monoid(..)
  , difference
  , These(..)
  , align
  , alignWith
  , mergeWith
  , leftmost
  , mergeList
  , mergeMap
  , fanMap
  , select
  , fanEither
  , fanThese
  , pushAlways
  , headE
  , tailE
  , headTailE
  , hold
  , sample
  , holdDyn
  , foldDyn
  , foldDynMaybe
  , foldDynM
  , foldDynMaybeM
  , count
  , toggle
  , distributeDMapOverDynPure
  , zipDynWith
  , demux
  , demuxed
  , holdUniqDynBy
  , traceDyn
  , traceDynWith
  , switchPromptly
  , switchPromptOnly
  , joinDynThroughMap
  , switcher
  , switchPromptlyDyn
  , module Data.These
  , DMap
  ) where

import Data.Align
import Data.These
import Game.Tulen.API.Camera
import Game.Tulen.API.Landscape
import Game.Tulen.API.Math
import Game.Tulen.API.Mouse
import Game.Tulen.API.Resource
import Reflex hiding (performEvent_, performEvent, getPostBuild)
import Reflex.Host.App
import Reflex.Host.Class
import Data.Dependent.Map (DMap)

-- | All partial APIs collected in one big API requirement
type TulenMonad t m = (
    MonadAppHost t m
  , MouseMonad t m
  , LandscapeMonad t m
  , CameraMonad t m
  )
