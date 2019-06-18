module Mixlogue.Slack.API where

import           RIO

import           Data.Extensible
import           Mixlogue.Env
import           Network.HTTP.Req

type Channel = Record
  '[ "id"   >: Text
   , "name" >: Text
   ]

type ChannelList = Record
  '[ "channels"          >: [Channel]
   , "response_metadata" >: Record '[ "next_cursor" >: Text ]
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

fetchChannels :: SlackToken -> RIO Env [Channel]
fetchChannels token = go [] <*> toCursor =<< get Nothing
  where
    get :: Maybe ChannelList -> RIO Env ChannelList
    get = getChannelList' token 200 . fmap toCursor

    toCursor :: ChannelList -> Text
    toCursor = view #next_cursor . view #response_metadata

    go :: [Channel] -> ChannelList -> Text -> RIO Env [Channel]
    go acc _ "" = pure acc
    go acc cs _ = go (cs ^. #channels <> acc) <*> toCursor =<< get (Just cs)

getChannelList' :: SlackToken -> Int -> Maybe Text -> RIO Env ChannelList
getChannelList' t l c = do
  logDebug (display $ "fetching slack channels" <> maybe mempty (" from " <>) c)
  runReq defaultHttpConfig $ responseBody <$> getChannelList t l c

getChannelList :: SlackToken -> Int -> Maybe Text -> Req (JsonResponse ChannelList)
getChannelList token limit cursor = req GET url NoReqBody jsonResponse params
  where
    url    = https "slack.com" /: "api" /: "channels.list"
    params = mconcat
      [ "token" =: token
      , "limit" =: limit
      , "exclude_archived" =: True
      , "exclude_members"  =: True
      , maybe mempty ("cursor" =:) cursor
      ]

fetchMessages :: SlackToken -> UnixTime -> Channel -> RIO Env [Message]
fetchMessages token ts ch =
  view #messages <$> getMessageList' token ts (ch ^. #id)

getMessageList' :: SlackToken -> UnixTime -> Text -> RIO Env MessageList
getMessageList' t ts cid = do
  logDebug (display $ "fetching slack messages from " <> cid)
  runReq defaultHttpConfig $ responseBody <$> getMessageList t ts cid

getMessageList :: SlackToken -> UnixTime -> Text -> Req (JsonResponse MessageList)
getMessageList token ts cid = req GET url NoReqBody jsonResponse params
  where
    url    = https "slack.com" /: "api" /: "channels.history"
    params = mconcat
      [ "token"   =: token
      , "channel" =: cid
      , "oldest"  =: ts
      , "inclusive" =: True
      ]
