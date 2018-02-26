module Game.Tulen.API.UI.Element(
    HorizontalAlignment(..)
  , VerticalAlignment(..)
  , UIElementCfg(..)
  , Parent(..)
  , IsUIElement(..)
  , UIElement(..)
  , UIElementMonad(..)
  -- * Events info
  , ClickInfo(..)
  , ClickEndInfo(..)
  , DragTestInfo(..)
  , DragFinishInfo(..)
  , DragInfo(..)
  , DragMoveInfo(..)
  , DragEndInfo(..)
  , DragCancelInfo(..)
  , HoverInfo(..)
  , HoverEndInfo(..)
  ) where

import GHC.Generics
import Reflex

import Game.Tulen.API.KeyBoard
import Game.Tulen.API.Math
import Game.Tulen.API.Mouse

-- | UI element alignment option
data HorizontalAlignment =
    AlignmentLeft
  | AlignmentHorizontalCenter
  | AlignmentRight
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | UI element alignment option
data VerticalAlignment =
    AlignmentTop
  | AlignmentVerticalCenter
  | AlignmentBottom
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Information about click on UI element
data ClickInfo = ClickInfo {
  clickInfoPos        :: !ViewPoint   -- ^ Screen position
, clickInfoButton     :: !MouseButton -- ^ Pressed button
, clickInfoQualifiers :: ![Qualifier] -- ^ Pressed qualifiers buttons
} deriving (Show, Generic)

-- | Information about click ended on UI element
data ClickEndInfo t m = ClickEndInfo {
  clickEndInfoBeginElem  :: !(UIElementRef t m)   -- ^ UI element where click began
, clickEndInfoPos        :: !ViewPoint   -- ^ Screen position
, clickEndInfoButton     :: !MouseButton -- ^ Pressed button
, clickEndInfoQualifiers :: ![Qualifier] -- ^ Pressed qualifiers buttons
} deriving (Generic)

deriving instance Show (UIElementRef t m) => Show (ClickEndInfo t m)

-- | Information about 'can perform drag-drop' test
data DragTestInfo t m = DragTestInfo {
  dragTestSource :: !(UIElementRef t m) -- ^ Start element
, dragTestTarget :: !(UIElementRef t m) -- ^ End element
, dragTestAccept :: !Bool -- ^ Result of test
} deriving (Generic)

deriving instance Show (UIElementRef t m) => Show (DragTestInfo t m)

-- | Information about 'can perform drag-drop' test
data DragFinishInfo t m = DragFinishInfo {
  dragFinishSource :: !(UIElementRef t m) -- ^ Start element
, dragFinishTarget :: !(UIElementRef t m) -- ^ End element
, dragFinishAccept :: !Bool -- ^ Result of test
} deriving (Generic)

deriving instance Show (UIElementRef t m) => Show (DragFinishInfo t m)

-- | Information about drag-drop start
data DragInfo t m = DragInfo {
  dragBeginElement    :: !(UIElementRef t m)
, dragBeginPos        :: !(V2 Int)
, dragBeginElementPos :: !(V2 Int)
, dragBeginButtons    :: !Int
, dragBeginNumButtons :: !Int
} deriving (Generic)

deriving instance Show (UIElementRef t m) => Show (DragInfo t m)

-- | Info about intermidieate drag-drop movement
data DragMoveInfo t m = DragMoveInfo {
  dragMoveElement    :: !(UIElementRef t m)
, dragMovePos        :: !(V2 Int)
, dragMoveDPos       :: !(V2 Int)
, dragMoveElementPos :: !(V2 Int)
, dragMoveButtons    :: !Int
, dragMoveNumButtons :: !Int
} deriving (Generic)

deriving instance Show (UIElementRef t m) => Show (DragMoveInfo t m)

-- | Info about intermidieate drag-drop movement
data DragEndInfo t m = DragEndInfo {
  dragEndElement    :: !(UIElementRef t m)
, dragEndPos        :: !(V2 Int)
, dragEndElementPos :: !(V2 Int)
, dragEndButtons    :: !Int
, dragEndNumButtons :: !Int
} deriving (Generic)

deriving instance Show (UIElementRef t m) => Show (DragEndInfo t m)

-- | Info about intermidieate drag-drop movement
data DragCancelInfo t m = DragCancelInfo {
  dragCancelElement    :: !(UIElementRef t m)
, dragCancelPos        :: !(V2 Int)
, dragCancelElementPos :: !(V2 Int)
, dragCancelButtons    :: !Int
, dragCancelNumButtons :: !Int
} deriving (Generic)

deriving instance Show (UIElementRef t m) => Show (DragCancelInfo t m)

-- | Info about hover begin event
data HoverInfo t m = HoverInfo {
  hoverElement         :: !(UIElementRef t m)
, hoverBeginPos        :: !(V2 Int)
, hoverBeginElementPos :: !(V2 Int)
} deriving (Generic)

deriving instance Show (UIElementRef t m) => Show (HoverInfo t m)

-- | Info about hover end event
data HoverEndInfo t m = HoverEndInfo {
  hoverEndElement :: !(UIElementRef t m)
} deriving (Generic)

deriving instance Show (UIElementRef t m) => Show (HoverEndInfo t m)

-- | Inputs of UI element
data UIElementCfg t m = UIElementCfg {
  uiElemCfgParent :: UIElementRef t m -- ^ Parent (use root for the top-level)
} deriving (Generic)

-- | Outputs of UI element
data UIElement t m = UIElement {
  uiElemName           :: Dynamic t String              -- ^ Current name
, uiElemPosition       :: Dynamic t (V2 Int)            -- ^ Current position
, uiElemSize           :: Dynamic t (V2 Int)            -- ^ Current size
, uiElemMinSize        :: Dynamic t (V2 Int)            -- ^ Current minimal size
, uiElemMaxSize        :: Dynamic t (V2 Int)            -- ^ Current maximum size
, uiElemFixedSize      :: Dynamic t (V2 Bool)           -- ^ Is element has fixed size (per component)
, uiElemColor          :: Dynamic t Color               -- ^ Current base color
, uiElemChildOffset    :: Dynamic t (V2 Int)            -- ^ Current child offset
, uiElemHAlign         :: Dynamic t HorizontalAlignment -- ^ Current horizontal aligment
, uiElemVAlign         :: Dynamic t VerticalAlignment   -- ^ Current vertical aligment
, uiElemOpacity        :: Dynamic t Double              -- ^ Current element opacity
, uiElemFocus          :: Dynamic t Bool                -- ^ Current focus of element
, uiElemEnabled        :: Dynamic t Bool                -- ^ Current enabled status
, uiElemEditable       :: Dynamic t Bool                -- ^ Whether element is editable at the moment
, uiElemVisible        :: Dynamic t Bool                -- ^ Whether element is visible at the moment
, uiElemHovering       :: Dynamic t Bool                -- ^ Whether element is in hovering state at the moment
, uiElemClick          :: Event t ClickInfo             -- ^ Fires when an user clicks on the element
, uiElemClickEnd       :: Event t (ClickEndInfo t m)    -- ^ Fires when an user ends click on the element
, uiElemDoubleClick    :: Event t ClickInfo             -- ^ Fires when an user double clicks on the element
, uiElemDragDropTest   :: Event t (DragTestInfo t m)    -- ^ Fires when a drag-drop is started
, uiElemDragDropFinish :: Event t (DragFinishInfo t m)  -- ^ Fires when a drag-drop is ended
, uiElemDragBegin      :: Event t (DragInfo t m)        -- ^ Fires when drag behavior over the element is started
, uiElemDragMove       :: Event t (DragMoveInfo t m)    -- ^ Fires when drag behavior over the element updates
, uiElemDragEnd        :: Event t (DragEndInfo t m)     -- ^ Fires when drag behavior over the element is ended
, uiElemDragCancel     :: Event t (DragCancelInfo t m)  -- ^ Fires when drag behavior is canceled by ESC
, uiElemHoverBegin     :: Event t (HoverInfo t m)       -- ^ Fires when hovering over the element is started
, uiElemHoverEnd       :: Event t (HoverEndInfo t m)    -- ^ Fires when hovering over the element is ended
} deriving (Generic)

-- | Defines parent-child inheritance relation.
-- >>> Parent a b
-- Means `a` is more general than `b` and `b` can be safely casted to `a`.
class Parent a b where
  -- | Cast child to parent
  toParent :: b -> a
  -- | Possible downcast from parent to child
  toChild :: a -> Maybe b

-- | Defines that `a` is kind of UI element that can be operated as generic
-- UI element.
class Parent (UIElement t m) (Element t m a) => IsUIElement t m a where
  -- | Info that should be passed to engine to create the UI element
  type ElementCreate t m a :: *
  -- | Element reference
  type Element t m a :: *

-- | Operations with UIElement
class Monad m => UIElementMonad t m | m -> t where
  -- | UI element reference
  type UIElementRef t m :: *

  -- | Get origin of UI hierarchy
  uiRootElement :: m (UIElement t m)

  -- | Creates new UI elements when input event fires
  createUIElement :: Event t (UIElementCfg t m) -- ^ Configuration of element
    -> m (Event t (UIElement t m)) -- ^ Return event that fires when the new element is ready
