module Mixlogue.Env where

import           RIO

import           Data.Extensible
import           Mix.Plugin.Logger ()

type Env = Record
  '[ "logger" >: LogFunc
   , "token"  >: SlackToken
   ]

type SlackToken = Text

type UnixTime = Text
