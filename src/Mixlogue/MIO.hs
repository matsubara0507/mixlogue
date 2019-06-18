module Mixlogue.MIO where

import           RIO

import           Control.Monad.Cont

eval :: MonadIO m => ContT r m r -> m r
eval = flip runContT pure

with :: MonadIO m => m (Maybe a) -> m r -> ContT r m a
with m e = lift m >>= \case
  Nothing -> ContT (const e)
  Just a  -> ContT ($ a)
