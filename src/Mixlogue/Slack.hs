module Mixlogue.Slack
  ( Channel
  , Message
  , User
  , UserId
  , fetchChannels
  , fetchMessages
  , fetchUser
  ) where

import           RIO

import           Data.Extensible
import           Mixlogue.Env
import           Mixlogue.Slack.API
import           Mixlogue.Slack.Utils (fromNullable, none)

fetchChannels :: SlackToken -> RIO Env [Channel]
fetchChannels token = go [] <*> toCursor =<< get Nothing
  where
    get :: Maybe ChannelList -> RIO Env ChannelList
    get = getChannelList' token . toParams 200 . fmap toCursor

    toCursor :: ChannelList -> Text
    toCursor = view #next_cursor . view #response_metadata

    go :: [Channel] -> ChannelList -> Text -> RIO Env [Channel]
    go acc _ "" = pure acc
    go acc cs _ = go (cs ^. #channels <> acc) <*> toCursor =<< get (Just cs)

    toParams :: Int -> Maybe Text -> ChannelListParams
    toParams l c =
      fromNullable none (wrench $ #limit @= Just l <: #cursor @= c <: nil)

fetchMessages :: SlackToken -> UnixTime -> Channel -> RIO Env [Message]
fetchMessages token ts ch = view #messages <$> getMessageList' token params
  where
    params  = (#channel @= ch ^. #id <: fromNullable none params')
    params' = wrench $ #oldest @= Just ts <: #inclusive @= Just True <: nil

fetchUser :: SlackToken -> UserId -> RIO Env User
fetchUser token uid = view #user <$> getUserInfo' token params
  where
    params = #user @= uid <: none
