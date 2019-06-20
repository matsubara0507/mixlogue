module Mixlogue.Slack.API where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.Logger.JSON as Mix
import           Mixlogue.Env
import           Mixlogue.Slack.Utils   (toQueryParam)
import           Network.HTTP.Req

type Channel = Record
  '[ "id"   >: Text
   , "name" >: Text
   ]

type ChannelList = Record
  '[ "channels"          >: [Channel]
   , "response_metadata" >: Record '[ "next_cursor" >: Text ]
   ]

type ChannelListParams = Record
  '[ "token"            >: Text
   , "cursor"           >: Maybe Text
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
  '[ "token"     >: Text
   , "channel"   >: Text
   , "count"     >: Maybe Int
   , "inclusive" >: Maybe Bool
   , "latest"    >: Maybe Text
   , "oldest"    >: Maybe Text
   , "unreads"   >: Maybe Bool
   ]

getChannelList :: ChannelListParams -> Req (JsonResponse ChannelList)
getChannelList = req GET url NoReqBody jsonResponse . toQueryParam
  where
    url = https "slack.com" /: "api" /: "channels.list"

getChannelList' :: ChannelListParams -> RIO Env ChannelList
getChannelList' params = do
  Mix.logDebugR "fetching slack channels" params
  runReq defaultHttpConfig $ responseBody <$> getChannelList params

getMessageList' :: MessageListParams -> RIO Env MessageList
getMessageList' params = do
  Mix.logDebugR "fetching slack messages" params
  runReq defaultHttpConfig $ responseBody <$> getMessageList params

getMessageList :: MessageListParams -> Req (JsonResponse MessageList)
getMessageList = req GET url NoReqBody jsonResponse . toQueryParam
  where
    url = https "slack.com" /: "api" /: "channels.history"
