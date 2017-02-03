module Main where

import Control.Lens 
import Control.Monad.Catch
import Control.Monad.Except
import Data.Proxy
import Data.Text
import Data.Text.Encoding
import Foreign.C
import System.IO 
import System.IO.Temp

import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Resource
import Game.GoreAndAsh.Time
import Game.GoreAndAsh.SDL

import qualified SDL
import qualified SDL.Image as SDLI

import SDL (get)

import qualified Data.ByteString.Lazy as BS

type AppStack t = ResourceT t (SDLT t (LoggingT t (TimerT t (GameMonad t))))

newtype AppMonad t a = AppMonad { runAppMonad :: AppStack t a}
  deriving (Functor, Applicative, Monad, MonadFix)

instance Resource Texture where
  type ResourceArg Texture = Renderer
  readResource r name bs = withSystemTempFile name $ \path h -> do 
    BS.hPut h bs 
    hFlush h
    img <- SDLI.loadTexture r path 
    return $ Right img -- TODO: handle exceptions and put them in Left

app :: forall t m . (TimerMonad t m, MonadResource t m, LoggingMonad t m, MonadSDL t m) => m ()
app = do
  tickE <- tickEveryN (realToFrac (1 :: Double)) 1 never
  logInfoE $ ffor tickE $ const "TIIIICK!"

  rec 
    let
      errE :: Event t Text
      errE = fforMaybe loadedE $ \e -> case e of
        Left s -> Just s
        _      -> Nothing

      succE :: Event t Texture
      succE = fforMaybe loadedE $ \e -> case e of
        Right s -> Just s
        _       -> Nothing

    logWarnE $ ffor errE showl
    texDyn <- holdDyn Nothing $ fmap Just succE
    win <- createMainWindow (const () <$> succE) (drawFrame texDyn) defaultWindowCfg
    loadedE <- loadResource $ ffor tickE $ const (win ^. windowRenderer, "kassa.png")

  return () 

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

deriving instance (ReflexHost t, MonadIO (HostFrame t), MonadCatch (HostFrame t)) => MonadSDL t (AppMonad t)



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




drawFrame :: forall t . (ReflexHost t, MonadIO (HostFrame t))
  => Dynamic t (Maybe Texture) -> Window -> Renderer -> HostFrame t ()
drawFrame texDyn win r = do
  rendererDrawColor r $= V4 0 0 0 0
  clear r
  mtex <- sample . current $ texDyn
  case mtex of 
    Nothing  -> return ()
    Just tex -> SDL.copy r tex Nothing Nothing
  glSwapWindow win
  where
    getCurrentSize :: HostFrame t (V2 CInt)
    getCurrentSize = do
      vp <- get (rendererViewport r)
      case vp of
        Nothing -> return 0
        Just (Rectangle _ s) -> return s

    resizeRect :: V2 CInt -> Rectangle Double -> Rectangle CInt
    resizeRect (V2 vw vh) (Rectangle (P (V2 x y)) (V2 w h)) = Rectangle (P (V2 x' y')) (V2 w' h')
      where
        x' = round $ x * fromIntegral vw
        y' = round $ y * fromIntegral vh
        w' = round $ w * fromIntegral vw
        h' = round $ h * fromIntegral vh
