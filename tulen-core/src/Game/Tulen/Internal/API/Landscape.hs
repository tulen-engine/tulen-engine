module Game.Tulen.Internal.API.Landscape(

  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Game.Tulen.Internal.API.Helpers
import Game.Tulen.Internal.ExternalRef
import Game.Tulen.Internal.Landscape
import Game.Tulen.Internal.Monad
import Graphics.Urho3D.Multithread
import Linear
import qualified Game.Tulen.API.Landscape as API
import qualified Game.Tulen.API.Math as API
import qualified Game.Tulen.API.Resource as API

import Data.Foldable (toList)

instance ToAPI (Int, TileInfo) API.TileInfo where
  toAPI (n, tinfo) = API.TileInfo {
      API.tileInfoId = API.TileId n
    , API.tileInfoName = tileResource tinfo
    , API.tileInfoPath = API.ResourceRef $ tileResource tinfo
    }
  {-# INLINE toAPI #-}

instance API.LandscapeMonad Spider TulenM where
  tileset = do
    loadLandD <- externalRefDynamic =<< asks coreLandscape
    holdUniqDyn $ ffor loadLandD $ \land -> let
      tiles = toList . landscapeTiles . loadedLandDatum $ land :: [TileInfo]
      tiles' = fmap toAPI $ [0 :: Int ..] `zip` tiles :: [API.TileInfo]
      in tiles'
  {-# INLINE tileset #-}

  tileSetter e = do
    core <- ask
    performEventAsync $ ffor e $ \(API.V2 x y, API.TileId n) -> flip runReaderT core $
      modifyExternalRefM (coreLandscape core) $ \land -> do
        let oldTile = landscapeGetTile (loadedLandDatum land) (V2 x y)
        landVar <- liftIO newEmptyMVar
        runInMainThread $ do
          land' <- updateLoadedLandscape (landscapeUpdateTiles (V2 x y) 1 (const $ const $ fromIntegral n)) land
          putMVar landVar land'
        land' <- liftIO $ readMVar landVar
        pure (land', API.TileId $ fromIntegral oldTile)
  {-# INLINE tileSetter #-}
