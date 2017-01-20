{-|
Module      : Game.GoreAndAsh.Resource
Description : Top module of game module for Gore&Ash engine for resource handling.
Copyright   : (c) Anton Gushcha, 2016-2017
                  Anatoly Nardid, 2016-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Resource(
    MonadResource(..)
  , Resource(..)
  , ResourceT(..)
  , ResourceOptions(..)
  ) where

import Game.GoreAndAsh.Resource.Module
import Game.GoreAndAsh.Resource.API