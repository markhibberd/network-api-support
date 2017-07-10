{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Network.Api.Support.Core (
  runRequest
, runRequest'
) where

import Network.Api.Support.Request
import Network.Api.Support.Response

import Control.Monad

import Data.Text
import Data.Monoid

import Network.HTTP.Client
import Network.HTTP.Types

-- * Request runners

-- | Run a request using the specified settings, method, url and request transformer.
runRequest ::
  ManagerSettings
  -> StdMethod
  -> Text
  -> RequestTransformer
  -> Responder b
  -> IO b
runRequest settings stdmethod url transform  =
  runRequest' settings url (transform <> setMethod (renderStdMethod stdmethod))

-- | Run a request using the specified settings, url and request transformer. The method
-- | can be set using the setMethod transformer. This is only useful if you require a
-- | custom http method. Prefer runRequest where possible.
runRequest' ::
  ManagerSettings
  -> Text
  -> RequestTransformer
  -> Responder b
  -> IO b
runRequest' settings url transform responder =
  do url' <- parseRequest $ unpack url
     let url'' = url' { checkStatus = const . const . const $ Nothing } -- handle all response codes.
     let req = appEndo transform url''
     manager <- newManager settings
     liftM (responder req) $ httpLbs req manager
