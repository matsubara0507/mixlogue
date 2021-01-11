module Mixlogue.Message
  ( Info
  , build
  ) where

import           RIO

import           Data.Extensible
import           Data.Fallible         (evalContT, exitA, (???))
import           Mixlogue.Cache        (Cache, with)
import           Mixlogue.Env
import           Mixlogue.Message.Type
import           Mixlogue.Slack        (Channel, Message, User, fetchUser)

build :: Cache -> Channel -> Message -> RIO Env (Maybe Info)
build cache ch msg = evalContT $ do
  userId <- msg ^. #user ??? exitA Nothing
  client <- asks (view #client)
  user   <- lift (fetchUser client `with` (cache ^. #users) $ userId)
  pure $ toInfo ch msg <$> user

toInfo :: Channel -> Message -> User -> Info
toInfo ch msg user
     = #user    @= user
    <: #text    @= (msg ^. #text)
    <: #channel @= ch
    <: #ts      @= (msg ^. #ts)
    <: nil
