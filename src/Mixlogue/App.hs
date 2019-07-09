module Mixlogue.App where

import           RIO

import           Mixlogue.Cache
import qualified Mixlogue.Message            as Message
import           Servant
import           Servant.HTML.Blaze
import           Servant.Server.StaticFiles  (serveDirectoryFileServer)
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as H

app :: Cache -> Application
app = serve api . server

type API = Get '[HTML] H.Html
      :<|> "static" :> Raw
      :<|> "api"    :> "messages" :> Get '[JSON] [Message.Info]

api :: Proxy API
api = Proxy

server :: Cache -> Server API
server cache = indexHtml
    :<|> serveDirectoryFileServer "static"
    :<|> readTVarIO (cache ^. #messages)
  where
    indexHtml = pure $ H.docTypeHtml $ do
      H.head $ stylesheet primerCss
      H.div ! H.id "main" $ H.text ""
      H.script ! H.src "static/main.js" $ H.text ""
      H.script ! H.src "static/index.js" $ H.text ""
    primerCss = "https://cdnjs.cloudflare.com/ajax/libs/Primer/11.0.0/build.css"

stylesheet :: H.AttributeValue -> H.Html
stylesheet url =
  H.link ! H.rel "stylesheet" ! H.type_ "text/css" ! H.href url ! H.media "all"
