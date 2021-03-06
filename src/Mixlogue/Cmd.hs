module Mixlogue.Cmd where

import           RIO
import qualified RIO.ByteString.Lazy      as BL
import           RIO.Directory            (doesFileExist)
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as Text

import qualified Data.Aeson               as J
import           Data.Extensible
import           Data.Fallible            (evalContT, exit, exitA, (!??), (???))
import qualified Mix.Plugin.Logger.JSON   as Mix
import           Mixlogue.App             (app)
import           Mixlogue.Cache           (Cache)
import qualified Mixlogue.Cache           as Cache
import           Mixlogue.Env
import qualified Mixlogue.Message         as Message
import qualified Mixlogue.Slack           as Slack
import qualified Network.Wai.Handler.Warp as Warp
import           UnliftIO.Concurrent      (forkIO)

run :: Cmd -> RIO Env ()
run (ShowTimestamp ts) = logInfo $ display ts
run ShowChannels       = showChannels
run (RunServer ts)     = watchMessages ts

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

data Cmd
  = ShowTimestamp Slack.TimeStamp
  | ShowChannels
  | RunServer Slack.TimeStamp

showChannels :: RIO Env ()
showChannels = do
  channels <- getChannelsWithLocalCache
  forM_ channels $ \c -> logInfo (display $ c ^. #name)

getChannelsWithLocalCache :: RIO Env [Slack.Channel]
getChannelsWithLocalCache = do
  path    <- cacheChannelsFilePath
  isExist <- doesFileExist path
  flag    <- asks (view #update_local_cache)
  when (not isExist || flag) $ do
    Mix.logDebugR "write channels to local cache" (#path @= path <: nil)
    BL.writeFile path =<< fmap J.encode fetchTimesChannels
  readChannels path

fetchTimesChannels :: RIO Env [Slack.Channel]
fetchTimesChannels =
  fmap (filter isTimes) $ Slack.fetchChannels =<< asks (view #client)
  where
    isTimes :: Slack.Channel -> Bool
    isTimes = Text.isPrefixOf "times_" . view #name

readChannels :: FilePath -> RIO Env [Slack.Channel]
readChannels path = evalContT $ do
  Mix.logDebugR "read channels from local cache" (#path @= path <: nil)
  J.decode <$> BL.readFile path !?? (logError emessage >> pure [])
  where
    emessage = "can't decode local cache as slack channels"

watchMessages :: Slack.TimeStamp -> RIO Env ()
watchMessages ts = do
  conf     <- asks (view #config)
  Mix.logDebugR "oldest timestamp" (#ts @= ts <: nil)
  channels <- getChannelsWithLocalCache
  cache    <- Cache.init ts channels
  _ <- forkIO $ forever $
    forM_ channels (withSleep (conf ^. #interval) . showMessages cache)
  logInfo "Please accsess to localhost:8080"
  runServer 8080 conf cache
  where
    withSleep n act = threadDelay (n * 1_000_000) >> act

runServer :: MonadIO m => Int -> Config -> Cache -> m ()
runServer port conf = liftIO . Warp.run port . app conf

showMessages :: Cache -> Slack.Channel -> RIO Env ()
showMessages cache ch = evalContT $ do
  lift $ Mix.logDebugR "show messages" ch
  client <- lift $ asks (view #client)
  ts     <- readOldest !?? exit (Mix.logWarnR "channel not found" ch)
  msgs   <- lift $ Slack.fetchMessages client ts ch
  ts'    <- nextTimestamp msgs ??? exitA ()
  atomically (modifyTVar' (cache ^. #latests) $ Map.insert (ch ^. #id) ts')
  infos  <- catMaybes <$> mapM (lift . Message.build cache ch) msgs
  forM_ infos $ \info -> Mix.logInfoR "slack message" info
  atomically $ modifyTVar' (cache ^. #messages) (updateMessages infos)
  where
    readOldest = Map.lookup (ch ^. #id) <$> readTVarIO (cache ^. #latests)

nextTimestamp :: [Slack.Message] -> Maybe Slack.TimeStamp
nextTimestamp msgs = do
  oldest <- L.maximumMaybe (view #ts <$> msgs)
  tshow . (+ 1) <$> toInt oldest
  where
    toInt :: Slack.TimeStamp -> Maybe Int64
    toInt = readMaybe . Text.unpack . Text.takeWhile (/= '.')

updateMessages :: [Message.Info] -> [Message.Info] -> [Message.Info]
updateMessages new old =
  L.take 100 $ L.reverse $ L.sortOn (view #ts) (new <> old)
