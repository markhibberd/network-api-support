module Network.Api.Support.HandleCodes (
  handleAllResponseCodes
) where

import Network.HTTP.Client

handleAllResponseCodes :: Request -> Request
handleAllResponseCodes url = url { checkStatus = const . const . const $ Nothing } 