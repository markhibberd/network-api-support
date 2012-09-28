{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Network.Api.Support.Core (
  (<&>)
, checkDomainOnly
, setApiKey
, setParams
, setMethod
, setPost
, setGet
, setDelete
, setPut
, setHeaders
, runRequest
, RequestTransformer
) where

import Control.Failure
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.CaseInsensitive
import Data.Certificate.X509 (X509)
import Data.Text
import Data.Monoid

import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.TLS (TLSCertificateUsage)
import Network.TLS.Extra (certificateVerifyDomain)

infixr 5 <&>

(<&>) :: Monoid m => m -> m -> m
(<&>) = mappend

checkDomainOnly :: B8.ByteString -> [X509] -> IO TLSCertificateUsage
checkDomainOnly host' certs = return $ certificateVerifyDomain (B8.unpack host') certs

withCustomManager :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m) =>
  ManagerSettings -> (Manager -> ResourceT m a) -> m a
withCustomManager settings f = runResourceT $
    allocate (newManager settings) closeManager >>= \(_, manager) -> f manager

type RequestTransformer m = Endo (Request (ResourceT m))

setApiKey :: B.ByteString -> RequestTransformer m
setApiKey key = Endo $ applyBasicAuth key ""

setParams :: (Monad m) => [(B.ByteString, B.ByteString)] -> RequestTransformer m
setParams params = Endo $ urlEncodedBody params

setMethod :: B.ByteString -> RequestTransformer m
setMethod m = Endo $ \r -> r { method = m }

setGet :: RequestTransformer m
setGet = setMethod "GET"

setPut :: RequestTransformer m
setPut = setMethod "PUT"

setPost :: RequestTransformer m
setPost = setMethod "POST"

setDelete :: RequestTransformer m
setDelete = setMethod "DELETE"

setHeaders :: [(CI Ascii, B.ByteString)] -> RequestTransformer m
setHeaders m = Endo $ \r -> r { requestHeaders = m }

runRequest ::
  (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, Failure HttpException m) =>
  ManagerSettings -> Text -> RequestTransformer m -> (Response BL.ByteString -> b) -> m b
runRequest settings url transform responder =
  parseUrl (unpack url) >>= \url' ->
  (liftM responder . withCustomManager settings . httpLbs) ((appEndo transform $ url' {
      checkStatus = const . const $ Nothing
    }))

