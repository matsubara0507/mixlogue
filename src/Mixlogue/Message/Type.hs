{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mixlogue.Message.Type where

import           RIO

import           Data.Extensible
import           Elm             (ElmType (..))
import           Language.Elm
import           Mixlogue.Env    (UnixTime)
import qualified Mixlogue.Slack  as Slack

type Info = Record
  '[ "user"    >: Slack.User
   , "text"    >: Text
   , "channel" >: Slack.Channel
   , "ts"      >: UnixTime
   ]

instance ElmType Info where
  toElmType = toElmRecordType "Message"
