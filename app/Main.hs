module Main where

import           Paths_mixlogue         (version)
import           RIO
import qualified RIO.ByteString         as B

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import           Mixlogue.Cmd
import           Mixlogue.Env
import           Version                (showVersion')

main :: IO ()
main = withGetOpt "[options] [input-file]" opts $ \r args -> do
  _ <- tryIO $ loadFile defaultConfig
  case (r ^. #version, listToMaybe args) of
    (True, _)      -> B.putStr $ fromString (showVersion' version) <> "\n"
    (_, Nothing)   -> error "please input config file path."
    (_, Just path) -> runCmd r path
  where
    opts = #version @= versionOpt
        <: #verbose @= verboseOpt
        <: nil

type Options = Record
  '[ "version" >: Bool
   , "verbose" >: Bool
   ]

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

runCmd :: Options -> FilePath -> IO ()
runCmd opts _path = Mix.run plugin cmd
  where
    plugin :: Mix.Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin logOpts
       <: nil
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
