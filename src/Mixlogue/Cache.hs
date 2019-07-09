module Mixlogue.Cache where

import           RIO
import qualified RIO.Map               as Map

import           Data.Extensible
import           Mixlogue.Env
import qualified Mixlogue.Message.Type as Message
import qualified Mixlogue.Slack.API    as Slack

type Cache = Record
  '[ "latests"  >: TVar (Map Slack.ChannelId UnixTime)
   , "users"    >: TVar (Map Slack.UserId Slack.User)
   , "messages" >: TVar ([Message.Info])
   ]

init :: MonadIO m => UnixTime -> [Slack.Channel] -> m Cache
init ts chs = hsequence
    $ #latests  <@=> newTVarIO (Map.fromList $ map (,ts) $ map (view #id) chs)
   <: #users    <@=> newTVarIO mempty
   <: #messages <@=> newTVarIO mempty
   <: nil

with :: (MonadIO m, Ord k) => (k -> m v) -> TVar (Map k v) -> k -> m v
with act cache key = Map.lookup key <$> readTVarIO cache >>= \case
  Just user -> pure user
  Nothing   -> do
    value <- act key
    atomically (modifyTVar' cache $ Map.insert key value)
    pure value
