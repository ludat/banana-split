module Preludat
    ( module Control.Monad.Fail
    , module Protolude
    , orElse
    , orElseMay
    , orElse_
    ) where

import Control.Monad.Fail (MonadFail (fail))

import Protolude hiding (orElse)


orElse :: Monad m => m (Either error a) -> (error -> m a) -> m a
orElse action recovery = action >>= either recovery pure

orElse_ :: Monad m => m (Either error a) -> m a -> m a
orElse_ action recovery = orElse action (const recovery)

orElseMay :: Monad m => m (Maybe a) -> m a -> m a
orElseMay action recovery = action >>= maybe recovery pure
