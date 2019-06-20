module Mixlogue.Cmd where

import           RIO
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as Text

import           Control.Monad.Trans.Cont (evalContT)
import           Data.Extensible
import qualified Mix.Plugin.Logger.JSON   as Mix
import           Mixlogue.Env
import           Mixlogue.Fallible        (exit, exit', (!??), (???))
import qualified Mixlogue.Slack           as Slack

run :: Cmd -> RIO Env ()
run (ShowTimestamp ts) = logInfo $ display ts
run ShowChannels       = showChannels
run (RunServer ts)     = watchMessages ts

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

data Cmd
  = ShowTimestamp UnixTime
  | ShowChannels
  | RunServer UnixTime

showChannels :: RIO Env ()
showChannels = do
  channels <- fetchTimesChannels
  forM_ channels $ \c -> logInfo (display $ c ^. #name)

fetchTimesChannels :: RIO Env [Slack.Channel]
fetchTimesChannels =
  fmap (filter isTimes) $ Slack.fetchChannels =<< asks (view #token)
  where
    isTimes :: Slack.Channel -> Bool
    isTimes = Text.isPrefixOf "times_" . view #name

watchMessages :: UnixTime -> RIO Env ()
watchMessages ts = do
  Mix.logDebugR "oldest timestamp" (#ts @= ts <: nil)
  channels <- fetchTimesChannels
  cache    <- newTVarIO (iniCache ts channels)
  forever $ forM_ channels (withSleep 5 . showMessages cache)
  where
    withSleep n act = threadDelay (n * 1_000_000) >> act

showMessages :: TVar Cache -> Slack.Channel -> RIO Env ()
showMessages cache ch = evalContT $ do
  lift $ Mix.logDebugR "show messages" ch
  token <- lift $ asks (view #token)
  ts    <- readOldest !?? exit (Mix.logWarnR "channel not found" ch)
  msgs  <- lift $ Slack.fetchMessages token ts ch
  ts'   <- L.maximumMaybe (view #ts <$> msgs) ??? exit' ()
  atomically (modifyTVar' cache $ Map.insert (ch ^. #id) $ ts' <> "1")
  forM_ msgs $ \m ->
    Mix.logInfoR "slack message" (#channel @= (ch ^. #name) <: m)
  where
    readOldest = Map.lookup (ch ^. #id) <$> readTVarIO cache

type Cache = Map Text UnixTime

iniCache :: UnixTime -> [Slack.Channel] -> Map Text UnixTime
iniCache ts = Map.fromList . map (,ts) . map (view #id)
