module Mixlogue.Cache where

import           RIO
import qualified RIO.Map            as Map

import           Data.Extensible
import           Mixlogue.Env
import qualified Mixlogue.Slack.API as Slack

type Cache = Record
  '[ "latests"  >: TVar (Map Slack.ChannelId UnixTime)
   , "users"    >: TVar (Map Slack.UserId Slack.User)
   , "messages" >: TVar ([Slack.Message])
   ]

init :: MonadIO m => UnixTime -> [Slack.Channel] -> m Cache
init ts chs = hsequence
    $ #latests  <@=> newTVarIO (Map.fromList $ map (,ts) $ map (view #id) chs)
   <: #users    <@=> newTVarIO mempty
   <: #messages <@=> newTVarIO mempty
   <: nil
