module Mixlogue.Slack.API
  ( ChannelId
  , Channel
  , ChannelList
  , ChannelListParams
  , Message
  , MessageList
  , MessageListParams
  , getChannelList
  , getChannelList'
  , getMessageList
  , getMessageList'
  ) where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.Logger.JSON as Mix
import           Mixlogue.Env
import           Mixlogue.Slack.Utils   (toQueryParam)
import           Network.HTTP.Req

type ChannelId = Text

type Channel = Record
  '[ "id"   >: ChannelId
   , "name" >: Text
   ]

type ChannelList = Record
  '[ "channels"          >: [Channel]
   , "response_metadata" >: Record '[ "next_cursor" >: Text ]
   ]

type ChannelListParams = Record
  '[ "cursor"           >: Maybe Text
   , "exclude_archived" >: Maybe Bool
   , "exclude_members"  >: Maybe Bool
   , "limit"            >: Maybe Int
   ]

type Message = Record
  '[ "type" >: Text
   , "user" >: Maybe Text
   , "text" >: Text
   , "ts"   >: UnixTime
   ]

type MessageList = Record
  '[ "messages" >: [Message]
   , "oldest"   >: UnixTime
   , "has_more" >: Bool
   ]

type MessageListParams = Record
  '[ "channel"   >: Text
   , "count"     >: Maybe Int
   , "inclusive" >: Maybe Bool
   , "latest"    >: Maybe Text
   , "oldest"    >: Maybe Text
   , "unreads"   >: Maybe Bool
   ]

getChannelList ::
  SlackToken -> ChannelListParams -> Req (JsonResponse ChannelList)
getChannelList token =
  req GET url NoReqBody jsonResponse . toQueryParam . (`with` token)
  where
    url = https "slack.com" /: "api" /: "channels.list"

getChannelList' :: SlackToken -> ChannelListParams -> RIO Env ChannelList
getChannelList' token params = do
  Mix.logDebugR "fetching slack channels" params
  runReq defaultHttpConfig $ responseBody <$> getChannelList token params

getMessageList ::
  SlackToken -> MessageListParams -> Req (JsonResponse MessageList)
getMessageList token =
  req GET url NoReqBody jsonResponse . toQueryParam . (`with` token)
  where
    url = https "slack.com" /: "api" /: "channels.history"

getMessageList' :: SlackToken -> MessageListParams -> RIO Env MessageList
getMessageList' token params = do
  Mix.logDebugR "fetching slack messages" params
  runReq defaultHttpConfig $ responseBody <$> getMessageList token params

with :: Record xs -> SlackToken -> Record ("token" >: SlackToken ': xs)
with r token = #token @= token <: r
