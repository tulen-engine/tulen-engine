module Game.Tulen.Internal.API.UI.Element(

  ) where

import Control.Concurrent.STM
import Control.Monad.STM
import Data.Bimap (Bimap)
import Foreign
import Game.Tulen.API.UI.Element hiding (UIElement)
import Game.Tulen.Internal.Core.Types
import Game.Tulen.Internal.Monad
import Graphics.Urho3D

import qualified Data.Bimap as BM
import qualified Game.Tulen.API.UI.Element as API

instance UIElementMonad Spider TulenM where
  type UIElementRef Spider TulenM = UIElementId

  -- | Get origin of UI hierarchy
  --uiRootElement :: m (UIElement t m)
  uiRootElement = do
    tvar <- asks coreUIElements
    ui <- asks coreUI
    root <- uiGetRoot ui
    i <- registerExistingElement tvar root
    pure API.UIElement {
        uiElemName = undefined
      , uiElemPosition = undefined
      , uiElemSize = undefined
      , uiElemMinSize = undefined
      , uiElemMaxSize = undefined
      , uiElemFixedSize = undefined
      , uiElemColor = undefined
      , uiElemChildOffset = undefined
      , uiElemHAlign = undefined
      , uiElemVAlign = undefined
      , uiElemOpacity = undefined
      , uiElemFocus = undefined
      , uiElemEnabled = undefined
      , uiElemEditable = undefined
      , uiElemVisible = undefined
      , uiElemHovering = undefined
      , uiElemClick = undefined
      , uiElemClickEnd = undefined
      , uiElemDoubleClick = undefined
      , uiElemDragDropTest = undefined
      , uiElemDragDropFinish = undefined
      , uiElemDragBegin = undefined
      , uiElemDragMove = undefined
      , uiElemDragEnd = undefined
      , uiElemDragCancel = undefined
      , uiElemHoverBegin = undefined
      , uiElemHoverEnd = undefined
      }
  -- | Creates new UI elements when input event fires
  --createUIElement :: Event t (UIElementCfg t m) -- ^ Configuration of element
  --  -> m (Event t (UIElement t m)) -- ^ Return event that fires when the new element is ready

-- | Register existing UI element in registry
registerExistingElement :: TVar (Bimap UIElementId (Ptr UIElement), Int)
  -> Ptr UIElement
  -> TulenM UIElementId
registerExistingElement tvar ptr = liftIO . atomically $ do
  (reg, n) <- readTVar tvar
  let !i = UIElementId n
      !reg' = BM.insert i ptr reg
      !n' = n+1
  writeTVar tvar (reg', n')
  pure i
