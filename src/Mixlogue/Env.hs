module Mixlogue.Env where

import           RIO
import           RIO.Directory

import           Data.Extensible
import           Mix.Plugin.Logger ()

type Env = Record
  '[ "logger" >: LogFunc
   , "token"  >: SlackToken
   , "update_local_cache" >: Bool
   ]

type SlackToken = Text

type UnixTime = Text

cacheChannelsFilePath :: MonadIO m => m FilePath
cacheChannelsFilePath = do
  cacheDir <- getXdgDirectory XdgCache "mixlogue"
  createDirectoryIfMissing True cacheDir
  pure $ cacheDir ++ "/channels.json"
