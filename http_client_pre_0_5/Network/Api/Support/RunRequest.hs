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
     let url'' = url' { checkStatus = const . const . const $ Nothing } -- handle all response codes.
     let req = appEndo transform url''
     manager <- newManager settings
     liftM (responder req) $ httpLbs req manager

handleAllResponseCodes :: Request -> Request
handleAllResponseCodes url = url { checkStatus = const . const . const $ Nothing } 