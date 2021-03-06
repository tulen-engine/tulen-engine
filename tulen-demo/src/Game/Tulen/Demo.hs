module Game.Tulen.Demo(
    runDemo
  ) where

import Game.Tulen.API
import Game.Tulen.Core

runDemo :: IO ()
runDemo = runCore defaultCoreConfig makeReactiveAp

makeReactiveAp :: TulenMonad t m => m ()
makeReactiveAp = landscapeEdit

-- | Edit tiles by mouse click
landscapeEdit :: TulenMonad t m => m ()
landscapeEdit = do
  camD <- currentCamera
  clickE <- onMouseClick LeftButton
  hitE <- fmap (fmapMaybe id) $ cameraRaycast $ current camD `attach` clickE
  heightSetter $ ffor hitE $ \HitPoint{..} -> let
    V3 x _ z = hitPointPosition
    d = 1
    in (Rect (x-d) (z-d) (x+d) (z+d), const (+0.1))
  -- tileSetter $ ffor hitE $ \HitPoint{..} -> let V3 x _ z = hitPointPosition
  --    in (V2 (round x) (round z), TileId 1)
  pure ()
