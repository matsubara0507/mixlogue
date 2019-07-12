{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           RIO

import           Data.Proxy       (Proxy (..))
import           Elm              (ElmType, Spec (Spec), specsToDir,
                                   toElmDecoderSource, toElmEncoderSource,
                                   toElmTypeSource)
import           Mixlogue.App     (GetMessagesAPI)
import qualified Mixlogue.Message as Message
import qualified Mixlogue.Slack   as Slack
import           Servant          ((:>))
import           Servant.Elm      (defElmImports, generateElmForAPI)
import           Shelly           (run_, shelly)

spec :: Spec
spec = Spec ["Generated", "API"] $ concat
            [ [defElmImports]
            , toElmTypeAll      (Proxy @ Slack.User)
            , toElmTypeAll      (Proxy @ Slack.Channel)
            , toElmTypeAll      (Proxy @ Message.Info)
            , generateElmForAPI (Proxy @ ("api" :> GetMessagesAPI))
            ]

toElmTypeAll :: ElmType a => Proxy a -> [Text]
toElmTypeAll proxy =
  [ toElmTypeSource    proxy
  , toElmDecoderSource proxy
  , toElmEncoderSource proxy
  ]

main :: IO ()
main = do
  specsToDir [spec] "elm-src"
  shelly $ do
    run_ "elm" ["make", "elm-src/Main.elm", "--output=static/main.js"]
    run_ "elm-format" ["--yes", "elm-src/Generated/"]
