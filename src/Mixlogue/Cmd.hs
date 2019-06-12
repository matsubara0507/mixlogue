module Mixlogue.Cmd where

import           RIO
import qualified RIO.Text           as Text

import           Mixlogue.Env
import qualified Mixlogue.Slack.API as Slack

run :: Cmd -> RIO Env ()
run ShowChannels = showChannels
run RunServer    = showNotImpl

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

data Cmd
  = ShowChannels
  | RunServer

showChannels :: RIO Env ()
showChannels = do
  token    <- asks (view #token)
  channels <- filter isTimes <$> Slack.fetchChannels token
  forM_ channels $ \c -> logInfo (display $ c ^. #name)
  where
    isTimes = Text.isPrefixOf "times_" . view #name
