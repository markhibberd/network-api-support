{-# LANGUAGE OverloadedStrings, FlexibleContexts, CPP #-}
module Network.Api.Support.Core (
  RequestTransformer
, setApiKey
, setParams
, setHeaders
, setMethod
, runRequest
, runRawRequest
, checkDomainOnly
#if __GLASGOW_HASKELL__ < 704
,  (<>)
#endif
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
setHeaders m = Endo $ \r -> r { requestHeaders = m }

-- | Set the request method to be the specified name.
setMethod :: B.ByteString -> RequestTransformer m
setMethod m = Endo $ \r -> r { method = m }

-- * Request runners

runRequest ::
  (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, Failure HttpException m) =>
  ManagerSettings
  -> StdMethod
  -> Text
  -> RequestTransformer m
  -> (Response BL.ByteString -> b)
  -> m b
runRequest settings stdmethod url transform  =
  runRawRequest settings url (transform <> setMethod (renderStdMethod stdmethod))

runRawRequest ::
  (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, Failure HttpException m) =>
  ManagerSettings
  -> Text
  -> RequestTransformer m
  -> (Response BL.ByteString -> b)
  -> m b
runRawRequest settings url transform responder =
  do url' <- parseUrl $ unpack url
     (liftM responder . withCustomManager settings . httpLbs) ((appEndo transform $ url' {
                                                                   checkStatus = const . const $ Nothing
      }))

-- * Manager tools

-- | A TLS validator that checks the domain only. Note that this means the validator
-- | will not check the cert chain, and can be used on systems where Data.Certificate.X509
-- | falls over as it does not have access to local root certs.
-- |
-- | ! Use with caution !
checkDomainOnly :: B8.ByteString -> [X509] -> IO TLSCertificateUsage
checkDomainOnly host' certs = return $ certificateVerifyDomain (B8.unpack host') certs

-- * Compatability

#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

-- Un-exposed tools --

withCustomManager :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m) => ManagerSettings -> (Manager -> ResourceT m a) -> m a
withCustomManager settings f = runResourceT $
    allocate (newManager settings) closeManager >>= \(_, manager) -> f manager
