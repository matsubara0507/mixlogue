module Mixlogue.Message
  ( Info
  , build
  ) where

import           RIO

import           Data.Extensible
import           Data.Fallible
import           Mixlogue.Cache        (Cache, with)
import           Mixlogue.Env
import           Mixlogue.Message.Type
import qualified Mixlogue.Slack        as Slack

build :: Cache -> Slack.Channel -> Slack.Message -> RIO Env (Maybe Info)
build cache ch msg = evalContT $ do
  uid   <- msg ^. #user ??? exitA Nothing
  token <- lift $ asks (view #token)
  user  <- lift $ Slack.fetchUser token `with` (cache ^. #users) $ uid
  pure $ Just
      $ #user    @= user
     <: #text    @= (msg ^. #text)
     <: #channel @= ch
     <: #ts      @= (msg ^. #ts)
     <: nil
