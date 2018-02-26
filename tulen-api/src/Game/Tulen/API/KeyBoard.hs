module Game.Tulen.API.KeyBoard(
    KeyBoardMonad(..)
  , Key(..)
  , Qualifier(..)
  ) where

import Game.Tulen.API.Camera
import Game.Tulen.API.Math
import GHC.Generics
import Reflex

-- | Special keys on keyboard like Ctrl, Alt, etc.
data Qualifier =
    QualShift
  | QualCtrl
  | QualAlt
  deriving (Generic, Eq, Ord, Enum, Bounded, Show, Read)

-- | Operations with keyboard input
class Monad m => KeyBoardMonad t m | m -> t

-- | Trackable keyboard keys
data Key =
    KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | Key0
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeyBackSpace
  | KeyTab
  | KeyReturn
  | KeyReturn2
  | KeyKPEnter
  | KeyShift
  | KeyCtrl
  | KeyAlt
  | KeyGUI
  | KeyPause
  | KeyCapsLock
  | KeyEscape
  | KeySpace
  | KeyPageUp
  | KeyPageDown
  | KeyEnd
  | KeyHome
  | KeyLeft
  | KeyUp
  | KeyRight
  | KeyDown
  | KeySelect
  | KeyPrintScreen
  | KeyInsert
  | KeyDelete
  | KeyLGUI
  | KeyRGUI
  | KeyApplication
  | KeyKP0
  | KeyKP1
  | KeyKP2
  | KeyKP3
  | KeyKP4
  | KeyKP5
  | KeyKP6
  | KeyKP7
  | KeyKP8
  | KeyKP9
  | KeyKPMultiply
  | KeyKPPlus
  | KeyKPMinus
  | KeyKPPeriod
  | KeyKPDivide
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyNumLockClear
  | KeyScrollLock
  | KeyLShift
  | KeyRShift
  | KeyLCtrl
  | KeyRCtrl
  | KeyLAlt
  | KeyRAlt
  deriving (Generic, Eq, Ord, Enum, Bounded, Show, Read)
