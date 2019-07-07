module Mixlogue.Cmd where

import           RIO
import qualified RIO.List               as L
import qualified RIO.Map                as Map
import qualified RIO.Text               as Text

import           Data.Extensible
import           Data.Fallible          (evalContT, exit, exitA, (!??), (???))
import qualified Mix.Plugin.Logger.JSON as Mix
import           Mixlogue.Cache         (Cache)
import qualified Mixlogue.Cache         as Cache
import           Mixlogue.Env
import qualified Mixlogue.Slack         as Slack

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
  cache    <- Cache.init ts channels
  forever $ forM_ channels (withSleep 5 . showMessages cache)
  where
    withSleep n act = threadDelay (n * 1_000_000) >> act

showMessages :: Cache -> Slack.Channel -> RIO Env ()
showMessages cache ch = evalContT $ do
  lift $ Mix.logDebugR "show messages" ch
  token <- lift $ asks (view #token)
  ts    <- readOldest !?? exit (Mix.logWarnR "channel not found" ch)
  msgs  <- lift $ Slack.fetchMessages token ts ch
  ts'   <- nextTimestamp msgs ??? exitA ()
  atomically (modifyTVar' (cache ^. #latests) $ Map.insert (ch ^. #id) ts')
  forM_ msgs $ \m ->
    Mix.logInfoR "slack message" (#channel @= (ch ^. #name) <: m)
  where
    readOldest = Map.lookup (ch ^. #id) <$> readTVarIO (cache ^. #latests)

nextTimestamp :: [Slack.Message] -> Maybe UnixTime
nextTimestamp msgs = do
  oldest <- L.maximumMaybe (view #ts <$> msgs)
  tshow . (+ 1) <$> toInt oldest
  where
    toInt :: UnixTime -> Maybe Int
    toInt = readMaybe . Text.unpack . Text.takeWhile (/= '.')
