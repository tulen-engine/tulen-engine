module Game.Tulen.Internal.API.Mouse(

  ) where

import Game.Tulen.Internal.Monad
import Game.Tulen.API.Mouse
import qualified Game.Tulen.API.Math as API
import Graphics.Urho3D

instance MouseMonad Spider TulenM where
  onMouseClick btn = do
    (e, trigger) <- newExternalEvent
    app <- asks coreApplication
    ui <- asks coreUI
    subscribeToEvent app $ \MouseButtonDown{..} -> do
      pos@(IntVector2 px py) <- uiGetCursorPosition ui
      let point = API.V2 px py
      case btn of
        LeftButton -> when (mouseButtonDownButton == mouseButtonLeft) . void $ trigger point
        RightButton -> when (mouseButtonDownButton == mouseButtonLeft) . void $ trigger point
        MiddleButton -> when (mouseButtonDownButton == mouseButtonMiddle) . void $ trigger point
        ExtraButton1 -> when (mouseButtonDownButton == mouseButtonX1) . void $ trigger point
        ExtraButton2 -> when (mouseButtonDownButton == mouseButtonX2) . void $ trigger point
    pure e
