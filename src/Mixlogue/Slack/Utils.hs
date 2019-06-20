module Mixlogue.Slack.Utils where

import           RIO

import           Data.Extensible
import           GHC.TypeLits             (symbolVal)
import           Network.HTTP.Req
import           Web.Internal.HttpApiData (ToHttpApiData)

toQueryParam ::
  Forall (KeyValue KnownSymbol ToHttpApiData) xs => Record xs -> Option scheme
toQueryParam =
  hfoldMapWithIndexFor (Proxy @ (KeyValue KnownSymbol ToHttpApiData)) $ \m x ->
  let k = fromString (symbolVal $ proxyAssocKey m) in k =: runIdentity (getField x)

class Optional a where
  none :: a

instance Optional (Maybe a) where
  none = Nothing

instance Optional ([a]) where
  none = []

instance Optional a => Optional (Identity a) where
  none = pure none

instance Forall (ValueIs Optional) xs => Optional (Record xs) where
  none = htabulateFor (Proxy @ (ValueIs Optional)) $ \_ -> Field none

fromNullable :: RecordOf h xs -> Nullable (Field h) :* xs -> RecordOf h xs
fromNullable def =
  hmapWithIndex $ \m x -> fromMaybe (hlookup m def) (getNullable x)
