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

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.CaseInsensitive
import Data.Monoid
import Data.Time

import Network.HTTP.Conduit
import Network.HTTP.Client (insertCookiesIntoRequest)

-- * Request transformers

-- | A RequestTransformer allows you to build up attributes on the request.
-- | RequestTransformer is simply an Endo, and therefore has a Monoid, so
-- | can be combined with `<>`.
type RequestTransformer = Endo Request

-- | Set an api key for use with basic auth.
setApiKey :: B.ByteString -> RequestTransformer
setApiKey key = Endo $ applyBasicAuth key ""

-- | Set request query parameters.
setParams :: [(B.ByteString, B.ByteString)] -> RequestTransformer
setParams params = Endo $ urlEncodedBody params

-- | Set request headers.
setHeaders :: [(CI B.ByteString, B.ByteString)] -> RequestTransformer
setHeaders hs = Endo $ \r -> r { requestHeaders = hs }

-- | Set a request headers.
setHeader :: (CI B.ByteString, B.ByteString) -> RequestTransformer
setHeader h = stripHeader (fst h) <> addHeader h

-- | Add a request headers.
addHeader :: (CI B.ByteString, B.ByteString) -> RequestTransformer
addHeader h = Endo $ \r -> r { requestHeaders = requestHeaders r ++ [h] }

-- | Set a request headers.
stripHeader :: CI B.ByteString -> RequestTransformer
stripHeader n = (Endo $ \r ->  r {
    requestHeaders = filter (\x -> fst x == n) (requestHeaders r)
  })

-- | Register all cookies in cookie jar against request.
setCookieJar :: CookieJar -> UTCTime -> RequestTransformer
setCookieJar cj now = Endo $ \r -> fst $ insertCookiesIntoRequest r cj now

-- | Set the request method to be the specified name.
setMethod :: B.ByteString -> RequestTransformer
setMethod m = Endo $ \r -> r { method = m }

-- | Set the request body from the specified byte string.
setBody :: B.ByteString -> RequestTransformer
setBody b = Endo $ \r -> r { requestBody = RequestBodyBS b }

-- | Set the request body from the specified lazy byte string.
setBodyLazy :: BL.ByteString -> RequestTransformer
setBodyLazy b = Endo $ \r -> r { requestBody = RequestBodyLBS b }

-- | Set the request body from the value which can be converted to JSON.
setJson :: ToJSON a => a -> RequestTransformer
setJson v = setHeader ("Content-Type", "application/json") <>
            (setBodyLazy . encode . toJSON $ v)

-- * Compatability

#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
