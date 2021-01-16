{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           RIO
import           RIO.FilePath        ((</>))

import           Elm.Mapping
import           Mixlogue.App        (GetMessagesAPI)
import           Mixlogue.Env        (Config)
import qualified Mixlogue.Message    as Message
import qualified Mixlogue.Slack      as Slack
import           Servant             ((:>))
import           Servant.Elm.Mapping (defElmImports, defElmOptions,
                                      generateElmModuleWith)
import           Shelly              (run_, shelly, test_px)
import           System.Environment  (getArgs)

main :: IO ()
main = do
  [dir] <- getArgs
  generateElmModuleWith
    defElmOptions
    ["Generated", "API"]
    defElmImports
    (dir </> "elm-src")
    [ DefineElm (Proxy @ Config)
    , DefineElm (Proxy @ Slack.User)
    , DefineElm (Proxy @ Slack.Channel)
    , DefineElm (Proxy @ Message.Info)
    ]
    (Proxy @ ("api" :> GetMessagesAPI))
  shelly $
    whenM (test_px "elm-format") $
      run_ "elm-format" ["--yes", fromString $ dir </> "elm-src/Generated/"]
