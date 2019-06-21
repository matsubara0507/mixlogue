module Mixlogue.Slack.Utils where

import           RIO

import           Data.Extensible
import           GHC.TypeLits             (symbolVal)
import           Network.HTTP.Req
import           Web.Internal.HttpApiData (ToHttpApiData)

toQueryParam ::
  Forall (KeyValue KnownSymbol ToQueryParam) xs => Record xs -> Option scheme
toQueryParam =
  hfoldMapWithIndexFor (Proxy @ (KeyValue KnownSymbol ToQueryParam)) $ \m x ->
  let k = fromString (symbolVal $ proxyAssocKey m) in k ==: runIdentity (getField x)

class ToHttpApiData a => ToQueryParam a where
  (==:) :: (QueryParam param, Monoid param) => Text -> a -> param

instance ToQueryParam Bool where
  (==:) = (=:)

instance ToQueryParam Text where
  (==:) = (=:)

instance ToQueryParam Int where
  (==:) = (=:)

instance ToQueryParam a => ToQueryParam (Maybe a) where
  _ ==: Nothing  = mempty
  k ==: (Just a) = k =: a

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
