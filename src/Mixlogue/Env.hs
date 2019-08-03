{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mixlogue.Env where

import           RIO
import           RIO.Directory

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import           Mix.Plugin.Logger           ()

type Env = Record
  '[ "logger" >: LogFunc
   , "token"  >: SlackToken
   , "config" >: Config
   , "update_local_cache" >: Bool
   ]

type SlackToken = Text

type UnixTime = Text

type Config = Record
  '[ "workspace" >: Maybe Text
   , "interval"  >: Int
   ]

instance IsElmType Config where
  compileElmType = compileElmRecordTypeWith "Config"

instance IsElmDefinition Config where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Config"

cacheChannelsFilePath :: MonadIO m => m FilePath
cacheChannelsFilePath = do
  cacheDir <- getXdgDirectory XdgCache "mixlogue"
  createDirectoryIfMissing True cacheDir
  pure $ cacheDir ++ "/channels.json"
