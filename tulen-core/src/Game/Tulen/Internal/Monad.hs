module Game.Tulen.Internal.Monad(
    TulenM
  , Core(..)
  , runTulenM
  -- * Reexports of control primitives
  , MonadAppHost
  , AppInfo
  , AppHost
  , HostFrame
  , module Reflex
  , newExternalEvent
  , performEventAsync
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
  ) where

import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.RSS.Strict
import Control.Monad.Writer
import Data.Semigroup.Applicative
import Reflex hiding (performEvent_, performEventAsync, performEvent, getPostBuild)
import Reflex.Host.App
import Reflex.Host.App.Internal
import Reflex.Host.Class
import Reflex.Spider

import Game.Tulen.Internal.Core.Types

import qualified Reflex.Spider.Internal as R

-- | Internal monad that implements public API
newtype TulenM a = TulenM { unTulenM :: ReaderT Core (AppHost Spider) a }
  deriving (Functor, Applicative, Monad
    , MonadHold Spider
    , MonadSample Spider
    , MonadFix
    , MonadIO
    , MonadSubscribeEvent Spider
    , MonadReflexCreateTrigger Spider
    , MonadReader Core
    , MonadThrow
    )

instance MonadAppHost Spider TulenM where
  getFireAsync = TulenM $ lift getFireAsync
  {-# INLINE getFireAsync #-}
  getRunAppHost = TulenM $ do
    runner <- lift getRunAppHost
    core <- ask
    pure $ runner . flip runReaderT core . unTulenM
  {-# INLINE getRunAppHost #-}
  performPostBuild_ = TulenM . lift . performPostBuild_
  {-# INLINE performPostBuild_ #-}
  liftHostFrame = TulenM . lift . liftHostFrame
  {-# INLINE liftHostFrame #-}

instance MonadThrow (R.EventM a) where
  throwM = R.EventM . throwM

instance MonadThrow (R.SpiderHostFrame a) where
  throwM = R.SpiderHostFrame . throwM

instance MonadThrow m => MonadThrow (RSST r w s m) where
  throwM = lift . throwM

instance (MonadThrow (HostFrame t), ReflexHost t) => MonadThrow (AppHost t) where
  throwM = AppHost . throwM

instance MonadCatch (R.EventM a) where
  catch (R.EventM m) f = R.EventM $ m `catch` (R.unEventM . f)

instance MonadCatch (R.SpiderHostFrame a) where
  catch (R.SpiderHostFrame m) f = R.SpiderHostFrame $ m `catch` (R.runSpiderHostFrame . f)

instance (Monoid w, MonadCatch m) => MonadCatch (RSST r w s m) where
  catch m f = do
    r <- ask
    s <- get
    (a, s', w) <- lift $ runRSST m r s `catch` (\e -> runRSST (f e) r s)
    put s'
    tell w
    return a

instance (MonadCatch (HostFrame t), ReflexHost t) => MonadCatch (AppHost t) where
  catch (AppHost m) f = AppHost $ m `catch` (unAppHost . f)

instance MonadCatch TulenM where
  catch (TulenM m) f = TulenM $ m `catch` (unTulenM . f)

instance (Monoid w, MonadMask m) => MonadMask (RSST r w s m) where
  mask m = do
    r <- ask
    s <- get
    (a, s', w) <- lift $ mask $ \u -> runRSST (m $ q u) r s
    put s'
    tell w
    return a
    where
      q :: (forall b . m b -> m b) -> RSST r w s m a -> RSST r w s m a
      q u m' = do
        r <- ask
        s <- get
        (a, s', w) <- lift $ u (runRSST m' r s)
        put s'
        tell w
        return a

  uninterruptibleMask m = do
    r <- ask
    s <- get
    (a, s', w) <- lift $ uninterruptibleMask $ \u -> runRSST (m $ q u) r s
    put s'
    tell w
    return a
    where
      q :: (forall b . m b -> m b) -> RSST r w s m a -> RSST r w s m a
      q u m' = do
        r <- ask
        s <- get
        (a, s', w) <- lift $ u (runRSST m' r s)
        put s'
        tell w
        return a

instance MonadMask (R.EventM s) where
  mask m = R.EventM $ mask $ \u -> R.unEventM (m $ q u)
    where
    q :: (forall b . IO b -> IO b) -> R.EventM s a -> R.EventM s a
    q u m' = R.EventM $ u (R.unEventM m')
  uninterruptibleMask m = R.EventM $ uninterruptibleMask $ \u -> R.unEventM (m $ q u)
    where
    q :: (forall b . IO b -> IO b) -> R.EventM s a -> R.EventM s a
    q u m' = R.EventM $ u (R.unEventM m')

instance MonadMask (R.SpiderHostFrame s) where
  mask m = R.SpiderHostFrame $ mask $ \u -> R.runSpiderHostFrame (m $ q u)
    where
    q :: (forall b . R.EventM s b -> R.EventM s b) -> R.SpiderHostFrame s a -> R.SpiderHostFrame s a
    q u m' = R.SpiderHostFrame $ u (R.runSpiderHostFrame m')
  uninterruptibleMask m = R.SpiderHostFrame $ uninterruptibleMask $ \u -> R.runSpiderHostFrame (m $ q u)
    where
    q :: (forall b . R.EventM s b -> R.EventM s b) -> R.SpiderHostFrame s a -> R.SpiderHostFrame s a
    q u m' = R.SpiderHostFrame $ u (R.runSpiderHostFrame m')

type AppHostStack t = RSST (AppEnv t) (Ap (HostFrame t) (AppInfo t)) () (HostFrame t)

instance (MonadMask (HostFrame t), ReflexHost t) => MonadMask (AppHost t) where
  mask m = AppHost $ mask $ \u -> unAppHost (m $ q u)
    where
    q :: (forall b . AppHostStack t b -> AppHostStack t b) -> AppHost t a -> AppHost t a
    q u m' = AppHost $ u (unAppHost m')
  uninterruptibleMask m = AppHost $ uninterruptibleMask $ \u -> unAppHost (m $ q u)
    where
    q :: (forall b . AppHostStack t b -> AppHostStack t b) -> AppHost t a -> AppHost t a
    q u m' = AppHost $ u (unAppHost m')

type TulenMStack = ReaderT Core (AppHost Spider)

instance MonadMask TulenM where
  mask m = TulenM $ mask $ \u -> unTulenM (m $ q u)
    where
    q :: (forall b . TulenMStack b -> TulenMStack b) -> TulenM a -> TulenM a
    q u m' = TulenM $ u (unTulenM m')
  uninterruptibleMask m = TulenM $ uninterruptibleMask $ \u -> unTulenM (m $ q u)
    where
    q :: (forall b . TulenMStack b -> TulenMStack b) -> TulenM a -> TulenM a
    q u m' = TulenM $ u (unTulenM m')

-- | Run the game monad, blocks until exit event occur.
runTulenM :: Core -> TulenM () -> IO ()
runTulenM core = runSpiderHost . hostApp . flip runReaderT core . unTulenM
