{-# LANGUAGE OverloadedStrings, CPP #-}
module Network.Api.Support.Request (
  RequestTransformer
, setApiKey
, setParams
, setHeaders
, setHeader
, addHeader
, stripHeader
, setCookieJar
, setMethod
, setBody
, setBodyLazy
, setJson
, (<>)
) where

import Control.Monad.Trans.Resource

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.CaseInsensitive
import Data.Monoid
import Data.Time

import Network.HTTP.Conduit
import Network.HTTP.Conduit.Internal (insertCookiesIntoRequest)

-- * Request transformers

-- | A RequestTransformer allows you to build up attributes on the request.
-- | RequestTransformer is simply an Endo, and therefore has a Monoid, so
-- | can be combined with `<>`.
type RequestTransformer m = Endo (Request (ResourceT m))

-- | Set an api key for use with basic auth.
setApiKey :: B.ByteString -> RequestTransformer m
setApiKey key = Endo $ applyBasicAuth key ""

-- | Set request query parameters.
setParams :: Monad m => [(B.ByteString, B.ByteString)] -> RequestTransformer m
setParams params = Endo $ urlEncodedBody params

-- | Set request headers.
setHeaders :: [(CI B.ByteString, B.ByteString)] -> RequestTransformer m
setHeaders hs = Endo $ \r -> r { requestHeaders = hs }

-- | Set a request headers.
setHeader :: (CI B.ByteString, B.ByteString) -> RequestTransformer m
setHeader h = stripHeader (fst h) <> addHeader h

-- | Add a request headers.
addHeader :: (CI B.ByteString, B.ByteString) -> RequestTransformer m
addHeader h = Endo $ \r -> r { requestHeaders = requestHeaders r ++ [h] }

-- | Set a request headers.
stripHeader :: CI B.ByteString -> RequestTransformer m
stripHeader n = (Endo $ \r ->  r {
    requestHeaders = filter (\x -> fst x == n) (requestHeaders r)
  })

-- | Register all cookies in cookie jar against request.
setCookieJar :: CookieJar -> UTCTime -> RequestTransformer m
setCookieJar cj now = Endo $ \r -> fst $ insertCookiesIntoRequest r cj now

-- | Set the request method to be the specified name.
setMethod :: B.ByteString -> RequestTransformer m
setMethod m = Endo $ \r -> r { method = m }

-- | Set the request body from the specified byte string.
setBody :: B.ByteString -> RequestTransformer m
setBody b = Endo $ \r -> r { requestBody = RequestBodyBS b }

-- | Set the request body from the specified lazy byte string.
setBodyLazy :: BL.ByteString -> RequestTransformer m
setBodyLazy b = Endo $ \r -> r { requestBody = RequestBodyLBS b }

-- | Set the request body from the value which can be converted to JSON.
setJson :: ToJSON a => a -> RequestTransformer m
setJson v = setHeader ("Content-Type", "application/json") <>
            (setBodyLazy . encode . toJSON $ v)

-- * Compatability

#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
