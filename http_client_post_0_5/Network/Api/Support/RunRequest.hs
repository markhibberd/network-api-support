{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Network.Api.Support.RunRequest (
  runRequest''
, handleAllResponseCodes
) where

import Network.Api.Support.Request
import Network.Api.Support.Response

import Control.Monad

import Data.Text
import Data.Monoid

import Network.HTTP.Client

runRequest'' ::
  ManagerSettings
  -> Text
  -> RequestTransformer
  -> Responder b
  -> IO b
runRequest'' settings url transform responder =
  do url' <- parseRequest $ unpack url
     let req = appEndo transform url'
     manager <- newManager settings
     liftM (responder req) $ httpLbs req manager

handleAllResponseCodes :: Request -> Request
handleAllResponseCodes = id -- From http_version 0.5 the default is to do nothing