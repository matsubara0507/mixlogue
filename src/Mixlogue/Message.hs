module Mixlogue.Message
  ( Info
  , build
  ) where

import           RIO
import qualified RIO.Map         as Map

import           Data.Extensible
import           Data.Fallible
import           Mixlogue.Cache  (Cache)
import           Mixlogue.Env
import qualified Mixlogue.Slack  as Slack

type Info = Record
  '[ "user"    >: Slack.User
   , "text"    >: Text
   , "channel" >: Slack.Channel
   , "ts"      >: UnixTime
   ]

build :: Cache -> Slack.Channel -> Slack.Message -> RIO Env (Maybe Info)
build cache ch msg = evalContT $ do
  uid  <- msg ^. #user ??? exitA Nothing
  user <- lift $ fetchUserWithCache cache uid
  pure $ Just
      $ #user    @= user
     <: #text    @= (msg ^. #text)
     <: #channel @= ch
     <: #ts      @= (msg ^. #ts)
     <: nil

fetchUserWithCache :: Cache -> Slack.UserId -> RIO Env Slack.User
fetchUserWithCache cache uid =
  Map.lookup uid <$> readTVarIO (cache ^. #users) >>= \case
    Just user -> pure user
    Nothing   -> do
      token <- asks (view #token)
      user  <- Slack.fetchUser token uid
      atomically (modifyTVar' (cache ^. #users) $ Map.insert uid user)
      pure user
