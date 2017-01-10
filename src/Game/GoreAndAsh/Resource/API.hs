module Game.GoreAndAsh.Resource.API(
    MonadResource(..)
  , Resource(..)
  ) where

import Control.Monad.Trans
import Data.ByteString.Lazy (ByteString)
import Data.Text
import Data.Typeable

import Game.GoreAndAsh

class Typeable a => Resource a where
  readResource :: FilePath -> ByteString -> Either Text a

class MonadAppHost t m => MonadResource t m | m -> t where
  loadResource :: Resource a => Event t FilePath -> m (Event t (Either Text a))

instance {-# OVERLAPPABLE #-} (MonadTrans mt, MonadAppHost t (mt m), MonadResource t m)
  => MonadResource t (mt m) where

  loadResource p = lift $ loadResource p
  {-# INLINE loadResource #-}
