module Main where

import           Paths_mixlogue         (version)
import           RIO
import qualified RIO.ByteString         as B
import qualified RIO.Time               as Time

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import           Mixlogue.Cmd           as Cmd
import qualified Mixlogue.Slack         as Slack
import           System.Environment     (getEnv)
import           Version                (showVersion')

main :: IO ()
main = withGetOpt "[options]" opts $ \r _args -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #version -> B.putStr $ fromString (showVersion' version) <> "\n"
     | r ^. #ts      -> runCmd r . Cmd.ShowTimestamp =<< toTimestamp (r ^. #before)
     | r ^. #ls      -> runCmd r Cmd.ShowChannels
     | otherwise     -> runCmd r . Cmd.RunServer =<< toTimestamp (r ^. #before)
  where
    opts = #version   @= versionOpt
        <: #verbose   @= verboseOpt
        <: #ls        @= lsOpt
        <: #update    @= updateOpt
        <: #ts        @= tsOpt
        <: #before    @= beforeOpt
        <: #interval  @= intervalOpt
        <: #workspace @= workspaceOpt
        <: nil

type Options = Record
  '[ "version"   >: Bool
   , "verbose"   >: Bool
   , "ls"        >: Bool
   , "update"    >: Bool
   , "ts"        >: Bool
   , "before"    >: Integer
   , "interval"  >: Int
   , "workspace" >: Maybe Text
   ]

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

lsOpt :: OptDescr' Bool
lsOpt = optFlag [] ["ls"] "Show target channels"

tsOpt :: OptDescr' Bool
tsOpt = optFlag [] ["ts"] "Show setting start timestamp"

beforeOpt :: OptDescr' Integer
beforeOpt =
  fromMaybe 60 . (readMaybe =<<) <$> optLastArg [] ["before"] "TIME" "Set what minutes ago to collect messages from"

intervalOpt :: OptDescr' Int
intervalOpt =
  fromMaybe 5 . (readMaybe =<<) <$> optLastArg [] ["interval"] "TIME" "Set interval second to collect messages"

workspaceOpt :: OptDescr' (Maybe Text)
workspaceOpt =
  (fmap fromString) <$> optLastArg [] ["workspace"] "TEXT" "Set slack workspace name for link"

updateOpt :: OptDescr' Bool
updateOpt = optFlag [] ["update"] "Update local cache: slack channels"

runCmd :: Options -> Cmd -> IO ()
runCmd opts cmd = do
  token <- liftIO $ fromString <$> getEnv "SLACK_TOKEN"
  let plugin = hsequence
         $ #logger <@=> MixLogger.buildPlugin logOpts
        <: #client <@=> pure (Slack.newClient token)
        <: #config <@=> pure (shrink opts)
        <: #update_local_cache <@=> pure (opts ^. #update)
        <: nil
  Mix.run plugin $ Cmd.run cmd
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil

toTimestamp :: Integer -> IO Slack.TimeStamp
toTimestamp before = do
  now <- Time.getCurrentTime
  let t = Time.addUTCTime (fromInteger $ -before * 60) now
  pure $ fromString $ Time.formatTime Time.defaultTimeLocale "%s%Q" t
