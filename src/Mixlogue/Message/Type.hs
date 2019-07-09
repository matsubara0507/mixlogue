module Mixlogue.Message.Type where

import           RIO

import           Data.Extensible
import           Mixlogue.Env    (UnixTime)
import qualified Mixlogue.Slack  as Slack

type Info = Record
  '[ "user"    >: Slack.User
   , "text"    >: Text
   , "channel" >: Slack.Channel
   , "ts"      >: UnixTime
   ]
