module Mixlogue.Cache where

import           RIO
import qualified RIO.Map               as Map

import           Data.Extensible
import qualified Mixlogue.Message.Type as Message
import qualified Mixlogue.Slack        as Slack

type Cache = Record
  '[ "latests"  >: TVar (Map Slack.ChannelID Slack.TimeStamp)
   , "users"    >: TVar (Map Slack.UserID Slack.User)
   , "messages" >: TVar ([Message.Info])
   ]

init :: MonadIO m => Slack.TimeStamp -> [Slack.Channel] -> m Cache
init ts chs = hsequence
    $ #latests  <@=> newTVarIO (Map.fromList $ map (,ts) $ map (view #id) chs)
   <: #users    <@=> newTVarIO mempty
   <: #messages <@=> newTVarIO mempty
   <: nil

with ::
  (MonadIO m, Ord k) => (k -> m (Maybe v)) -> TVar (Map k v) -> k -> m (Maybe v)
with act cache key = Map.lookup key <$> readTVarIO cache >>= \case
  Just user -> pure (Just user)
  Nothing   -> act key >>= \case
    Nothing   -> pure Nothing
    Just user -> do
      atomically (modifyTVar' cache $ Map.insert key user)
      pure (Just user)
