{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Network.Api.Support.Core (
  runRequest
, runRequest'
) where

import Network.Api.Support.Request
import Network.Api.Support.Response

import Control.Failure
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Data.Text
import Data.Monoid

import Network.HTTP.Conduit
import Network.HTTP.Types

-- * Request runners

-- | Run a request using the specified settings, method, url and request transformer.
runRequest ::
  (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, Failure HttpException m) =>
  ManagerSettings
  -> StdMethod
  -> Text
  -> RequestTransformer m
  -> Responder m b
  -> m b
runRequest settings stdmethod url transform  =
  runRequest' settings url (transform <> setMethod (renderStdMethod stdmethod))

-- | Run a request using the specified settings, url and request transformer. The method
-- | can be set using the setMethod transformer. This is only useful if you require a
-- | custom http method. Prefer runRequest where possible.
runRequest' ::
  (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, Failure HttpException m) =>
  ManagerSettings
  -> Text
  -> RequestTransformer m
  -> Responder m b
  -> m b
runRequest' settings url transform responder =
  do url' <- parseUrl $ unpack url
     let url'' = url' { checkStatus = const . const $ Nothing } -- handle all response codes.
     let req = appEndo transform url''
     liftM (responder req) . withCustomManager settings . httpLbs $ req

-- Build a custom connection manager and run ResourceT against that connection manager.
-- This function is responsible for ensuring manager is always closed.
withCustomManager ::
  (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m) =>
  ManagerSettings ->
  (Manager -> ResourceT m a) ->
  m a
withCustomManager settings f = runResourceT $
    allocate (newManager settings) closeManager >>= \(_, manager) -> f manager
