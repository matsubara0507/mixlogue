{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mixlogue.Slack
  ( Channel
  , Slack.ChannelID
  , Message
  , User
  , Slack.UserID
  , Slack.TimeStamp
  , Slack.newClient
  , fetchChannels
  , fetchMessages
  , fetchUser
  ) where

import           RIO

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import           Mixlogue.Env
import qualified Web.Slack                      as Slack
import           Web.Slack.WebAPI.Conversations (Conversations)
import qualified Web.Slack.WebAPI.Conversations as Conversations
import qualified Web.Slack.WebAPI.Users         as Users

type User = Record
  '[ "id"    >: Slack.UserID
   , "name"  >: Text
   , "color" >: Maybe Slack.ColorCode
   ]

instance IsElmType User where
  compileElmType = compileElmRecordTypeWith "User"

instance IsElmDefinition User where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "User"

type Channel = Record
  '[ "id"   >: Slack.ChannelID
   , "name" >: Text
   ]

instance IsElmType Channel where
  compileElmType = compileElmRecordTypeWith "Channel"

instance IsElmDefinition Channel where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Channel"

type Message = Record
  '[ "type" >: Text
   , "user" >: Maybe Slack.UserID
   , "text" >: Text
   , "ts"   >: Slack.TimeStamp
   ]

fetchChannels :: Slack.Client c => c -> RIO Env [Channel]
fetchChannels client = go [] Nothing
  where
    go acc (Just "") = pure acc
    go acc cursor    = do
      result <- Slack.runWebApi (Conversations.list client $ mkParams 200 cursor)
      let channels = either (pure []) toChannels result
          cursor'  = either (pure "") toCursor result
      go (channels <> acc) (Just cursor')

    mkParams :: Int -> Maybe Text -> Conversations.ListParams
    mkParams l = \case
      Just c  -> wrench (#cursor @= c <: params)
      Nothing -> wrench params
      where
        params = #limit @= l
              <: #exclude_archived @= True
              <: #types @= [Slack.PublicChannel, Slack.PrivateChannel]
              <: nil

    toChannels :: Conversations -> [Channel]
    toChannels cnvs = shrink <$> mapMaybe Slack.toChannel (cnvs ^. #channels)

    toCursor :: Conversations -> Text
    toCursor = maybe "" (view #next_cursor) . view #response_metadata

fetchMessages ::
  Slack.Client c => c -> Slack.TimeStamp -> Channel -> RIO Env [Message]
fetchMessages client ts ch = do
  result <- Slack.runWebApi (Conversations.history client (ch ^. #id) params)
  pure $ either (const []) (map shrink . view #messages) result
  where
    params = wrench $ #oldest @= ts <: #inclusive @= True <: nil

fetchUser :: Slack.Client c => c -> Slack.UserID -> RIO Env (Maybe User)
fetchUser client uid = do
  result <- Slack.runWebApi (Users.info client uid vacancy)
  pure $ either (const Nothing) (Just . shrink . view #user) result
