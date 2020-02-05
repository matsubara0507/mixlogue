{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mixlogue.Message.Type where

import           RIO

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import qualified Mixlogue.Slack              as Slack

type Info = Record
  '[ "user"    >: Slack.User
   , "text"    >: Text
   , "channel" >: Slack.Channel
   , "ts"      >: Slack.TimeStamp
   ]

instance IsElmType Info where
  compileElmType = compileElmRecordTypeWith "Message"

instance IsElmDefinition Info where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Message"
