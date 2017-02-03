{-|
Module      : Game.GoreAndAsh.Resource.Module
Description : Internal implementation of public API of resource game module
Copyright   : (c) Anton Gushcha, 2016-2017
                  Anatoly Nardid, 2016-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module defines implementation of resource game module. You are
interested only in a 'ResourceT' and 'ResourceOptions' types as 'ResourceT'
should be placed in your monad stack to enable 'MonadResource' API in your
application.

@
type AppStack t = ResourceT t (LoggingT t (TimerT t (GameMonad t)))

newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)
@

And you will need some boilerplate code for instance deriving, see
`examples/Example01.hs` for full example.

-}
module Game.GoreAndAsh.Resource.Module(
    ResourceOptions(..)
  , ResourceT(..)
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Monoid
import Data.Proxy
import Data.Text

import Game.GoreAndAsh
import Game.GoreAndAsh.Resource.API

import qualified Data.ByteString.Lazy as BS
import qualified Data.Dynamic as D

-- | Options that are passed to 'runModule' at application startup.
--
-- [@s@] The nested options of next module in stack. Options are layered the
-- similar way as parts of monad transformers.
data ResourceOptions s = ResourceOptions {
  resourceOptsPrefix :: FilePath -- ^ Folder where to search local resources. TODO: move this to resource pack
, resourceOptsNext   :: s -- ^ Nested options of next game module
}

-- | Callback that fires an event about finish of loading of resource
type ResourceLoadedTrigger = Either Text D.Dynamic -> IO Bool

-- | Internal environment of game module
data ResourceEnv t = ResourceEnv {
  -- | Options that were used to create the module
  resourceEnvOptions    :: ResourceOptions ()
  -- | Reading end of event that is exposed to FRP network
, resourceEnvLoaded     :: Event t (Either Text D.Dynamic)
  -- | Writing end of event that is kept internal
, resourceEnvFireLoaded :: ResourceLoadedTrigger
}

-- | Create a new environment for game module
newResourceEnv :: MonadAppHost t m => ResourceOptions s -> m (ResourceEnv t)
newResourceEnv opts = do
  (loadedE, loadedTrigger) <- newExternalEvent
  return ResourceEnv {
      resourceEnvOptions = opts { resourceOptsNext = () }
    , resourceEnvLoaded  = loadedE
    , resourceEnvFireLoaded = loadedTrigger
    }

-- | Implementation of 'MonadResource' API.
--
-- [@t@] FRP engine, you could ignore this parameter as it resolved only at main
-- function of your application.
--
-- [@m@] Underlying game modules, next layer in monad stack.
--
-- [@a@] Result of computation.
--
-- How to embed the monad into your app:
--
-- @
-- type AppStack t = ResourceT t (LoggingT t (TimerT t (GameMonad t)))
--
-- newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
--   deriving (Functor, Applicative, Monad, MonadFix)
-- @
--
-- And you will need some boilerplate code for instance deriving, see
-- `examples/Example01.hs` for full example.
--
newtype ResourceT t m a = ResourceT { runResourceT :: ReaderT (ResourceEnv t) m a }
  deriving (Functor, Applicative, Monad, MonadReader (ResourceEnv t), MonadFix
    , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadSample t, MonadHold t)

instance {-# OVERLAPPING #-} MonadAppHost t m => MonadResource t (ResourceT t m) where
  loadResource :: forall a . Resource a => Event t (ResourceArg a, FilePath) -> ResourceT t m (Event t (Either Text a))
  loadResource optsE = do
    ResourceEnv {..} <- ask
    let ResourceOptions {..} = resourceEnvOptions
    performEvent_ $ ffor optsE $ \(arg, name) -> do
      bs <- liftIO $ BS.readFile $ resourceOptsPrefix <> "/" <> name
      ma :: Either Text a <- liftIO $ readResource arg name bs
      _ <- liftIO $ resourceEnvFireLoaded $ fmap D.toDyn ma
      return ()
    return $ fforMaybe resourceEnvLoaded $ sequence . fmap D.fromDynamic

  {-# INLINE loadResource #-}

-- Boilerplate

instance MonadTrans (ResourceT t) where
  lift = ResourceT . lift

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ResourceT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (ResourceT t m) where
  subscribeEvent = lift . subscribeEvent

instance MonadAppHost t m => MonadAppHost t (ResourceT t m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- ResourceT getRunAppHost
    return $ \m -> runner $ runResourceT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadTransControl (ResourceT t) where
  type StT (ResourceT t) a = StT (ReaderT (ResourceEnv t)) a
  liftWith = defaultLiftWith ResourceT runResourceT
  restoreT = defaultRestoreT ResourceT

instance MonadBase b m => MonadBase b (ResourceT t m) where
  liftBase = ResourceT . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (ResourceT t m) where
  type StM (ResourceT t m) a = ComposeSt (ResourceT t) m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (ResourceT t m) where
  type ModuleOptions t (ResourceT t m) = ResourceOptions (ModuleOptions t m)
  runModule opts (ResourceT m) = do
    s <- newResourceEnv opts
    runModule (resourceOptsNext opts) $ runReaderT m s
  withModule t _ = withModule t (Proxy :: Proxy m)

