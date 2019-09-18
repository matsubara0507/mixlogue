{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           RIO

import           Data.Proxy          (Proxy (..))
import           Elm.Mapping
import           Mixlogue.App        (GetMessagesAPI)
import           Mixlogue.Env        (Config)
import qualified Mixlogue.Message    as Message
import qualified Mixlogue.Slack      as Slack
import           Servant             ((:>))
import           Servant.Elm.Mapping (defElmImports, defElmOptions,
                                      generateElmModuleWith)
import           Shelly              (run_, shelly)

main :: IO ()
main = do
  generateElmModuleWith
    defElmOptions
    ["Generated", "API"]
    defElmImports
    "elm-src"
    [ DefineElm (Proxy @ Config)
    , DefineElm (Proxy @ Slack.User)
    , DefineElm (Proxy @ Slack.Channel)
    , DefineElm (Proxy @ Message.Info)
    ]
    (Proxy @ ("api" :> GetMessagesAPI))
  shelly $ do
    run_ "elm" ["make", "elm-src/Main.elm", "--output=static/main.js"]
    run_ "elm-format" ["--yes", "elm-src/Generated/"]
