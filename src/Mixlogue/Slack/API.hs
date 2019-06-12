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
      , maybe mempty ("cursor" =:) cursor
      ]
