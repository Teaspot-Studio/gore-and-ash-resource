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

data ResourceOptions s = ResourceOptions {
  resourceOptsPrefix :: FilePath
, resourceOptsNext   :: s
}

type ResourceLoadedTrigger = Either Text D.Dynamic -> IO Bool

data ResourceEnv t = ResourceEnv {
  resourceEnvOptions    :: ResourceOptions ()
, resourceEnvLoaded     :: Event t (Either Text D.Dynamic)
, resourceEnvFireLoaded :: ResourceLoadedTrigger
}

newResourceEnv :: MonadAppHost t m => ResourceOptions s -> m (ResourceEnv t)
newResourceEnv opts = do
  (loadedE, loadedTrigger) <- newExternalEvent
  return ResourceEnv {
      resourceEnvOptions = opts { resourceOptsNext = () }
    , resourceEnvLoaded  = loadedE
    , resourceEnvFireLoaded = loadedTrigger
    }

newtype ResourceT t m a = ResourceT { runResourceT :: ReaderT (ResourceEnv t) m a }
  deriving (Functor, Applicative, Monad, MonadReader (ResourceEnv t), MonadFix
    , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadSample t, MonadHold t)

instance {-# OVERLAPPING #-} MonadAppHost t m => MonadResource t (ResourceT t m) where
  loadResource :: forall a . Resource a => Event t FilePath -> ResourceT t m (Event t (Either Text a))
  loadResource nameE = do
    ResourceEnv {..} <- ask
    let ResourceOptions {..} = resourceEnvOptions
    performEvent_ $ ffor nameE $ \name -> do
      bs <- liftIO $ BS.readFile $ resourceOptsPrefix <> "/" <> name
      let
        ma :: Either Text a
        ma = readResource name bs
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

