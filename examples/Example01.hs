module Main where

import Control.Monad.Catch
import Control.Monad.Except
import Data.Proxy
import Data.Text
import Data.Text.Encoding

import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Resource
import Game.GoreAndAsh.Time

import qualified Data.ByteString.Lazy as BS

type AppStack t = ResourceT t (LoggingT t (TimerT t (GameMonad t)))

newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)

newtype ASCIITexture = ASCIITexture Text

instance Show ASCIITexture where
  show (ASCIITexture t) = "\n" ++ unpack t

instance Resource ASCIITexture where
  readResource _ = Right . ASCIITexture . decodeUtf8 . BS.toStrict

app :: forall t m . (TimerMonad t m, MonadResource t m, LoggingMonad t m) => m ()
app = do
  tickE <- tickEveryN (realToFrac (1 :: Double)) 3 never
  --logInfoE $ ffor tickE $ const "TIIIICK!"
  loadedE <- loadResource $ ffor tickE $ const "texture.txt"
  let
    errE :: Event t Text
    errE = fforMaybe loadedE $ \e -> case e of
      Left s -> Just s
      _      -> Nothing

    succE :: Event t ASCIITexture
    succE = fforMaybe loadedE $ \e -> case e of
      Right s -> Just s
      _       -> Nothing

  logWarnE $ ffor errE showl
  logInfoE $ ffor succE showl

main :: IO ()
main = runSpiderHost $ hostApp $ runModule opts (app :: AppMonad Spider ())

opts :: ResourceOptions ()
opts = ResourceOptions {
    resourceOptsPrefix = "./media"
  , resourceOptsNext = ()
  }

-- Boilerplate below

deriving instance (ReflexHost t, MonadCatch (HostFrame t)) => MonadCatch (AppMonad t)
deriving instance (ReflexHost t, MonadThrow (HostFrame t)) => MonadThrow (AppMonad t)
deriving instance (ReflexHost t, MonadMask (HostFrame t)) => MonadMask (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadResource t (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => TimerMonad t (AppMonad t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => LoggingMonad t (AppMonad t)
deriving instance (ReflexHost t) => MonadSample t (AppMonad t)
deriving instance (ReflexHost t) => MonadHold t (AppMonad t)
deriving instance (ReflexHost t) => MonadSubscribeEvent t (AppMonad t)

instance ReflexHost t => MonadReflexCreateTrigger t (AppMonad t) where
  newEventWithTrigger = AppMonad . newEventWithTrigger
  newFanEventWithTrigger trigger = AppMonad $ newFanEventWithTrigger trigger

instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppMonad t) where
  getFireAsync = AppMonad getFireAsync
  getRunAppHost = do
    runner <- AppMonad getRunAppHost
    return $ \m -> runner $ runAppMonad m
  performPostBuild_ = AppMonad . performPostBuild_
  liftHostFrame = AppMonad . liftHostFrame

instance (ReflexHost t, MonadIO (HostFrame t)) => GameModule t (AppMonad t) where
  type ModuleOptions t (AppMonad t) = ModuleOptions t (AppStack t)
  runModule os m = runModule os $ runAppMonad m
  withModule t _ = withModule t (Proxy :: Proxy (AppStack t))
