module Game.Tulen.Demo(
    runDemo
  ) where

import Game.Tulen.Core

runDemo :: IO ()
runDemo = runCore defaultCoreConfig
