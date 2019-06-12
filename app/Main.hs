module Main where

import           Paths_mixlogue         (version)
import           RIO
import qualified RIO.ByteString         as B

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import           Mixlogue.Cmd           as Cmd
import           System.Environment     (getEnv)
import           Version                (showVersion')

main :: IO ()
main = withGetOpt "[options]" opts $ \r _args -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #version -> B.putStr $ fromString (showVersion' version) <> "\n"
     | r ^. #ls      -> runCmd r Cmd.ShowChannels
     | otherwise     -> runCmd r Cmd.RunServer
  where
    opts = #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #ls      @= lsOpt
        <: nil

type Options = Record
  '[ "version" >: Bool
   , "verbose" >: Bool
   , "ls"      >: Bool
   ]

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

lsOpt :: OptDescr' Bool
lsOpt = optFlag [] ["ls"] "Show target channels"

runCmd :: Options -> Cmd -> IO ()
runCmd opts cmd = do
  token <- liftIO $ fromString <$> getEnv "SLACK_TOKEN"
  let plugin = hsequence
         $ #logger <@=> MixLogger.buildPlugin logOpts
        <: #token  <@=> pure token
        <: nil
  Mix.run plugin $ Cmd.run cmd
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
