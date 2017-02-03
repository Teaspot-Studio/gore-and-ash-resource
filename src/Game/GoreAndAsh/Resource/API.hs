{-|
Module      : Game.GoreAndAsh.Resource.API
Description : API of resource module
Copyright   : (c) Anton Gushcha, 2016-2017
                  Anatoly Nardid, 2016-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains public API fo resource module. The module task is handle
loading resource of different types, caching them and grouping in resource packs.
-}
module Game.GoreAndAsh.Resource.API(
    MonadResource(..)
  , Resource(..)
  ) where

import Control.Monad.Trans
import Data.ByteString.Lazy (ByteString)
import Data.Text
import Data.Typeable

import Game.GoreAndAsh

-- | Describes a resource that can be loaded from raw bytes.
class Typeable a => Resource a where
  type ResourceArg a :: *
  readResource :: ResourceArg a -- ^ Specific argument for a
    -> FilePath -- ^ Path to resource inside a resource pack
    -> ByteString -- ^ Raw bytes of the resource
    -> IO (Either Text a) -- ^ Either error or loaded resource

-- | Public API of resouce module.
--
-- You can use like a mtl type class:
--
-- @
-- foo :: (MonadResource t m, LoggingMonad t m) => m ()
-- @
class MonadAppHost t m => MonadResource t m | m -> t where
  loadResource :: Resource a => Event t (ResourceArg a, FilePath) -> m (Event t (Either Text a))

instance {-# OVERLAPPABLE #-} (MonadTrans mt, MonadAppHost t (mt m), MonadResource t m)
  => MonadResource t (mt m) where

  loadResource p = lift $ loadResource p
  {-# INLINE loadResource #-}
