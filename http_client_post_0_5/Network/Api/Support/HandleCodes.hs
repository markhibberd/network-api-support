module Network.Api.Support.HandleCodes (
  handleAllResponseCodes
) where

import Network.HTTP.Client

handleAllResponseCodes :: Request -> Request
handleAllResponseCodes = id -- From http_version 0.5 the default is to do nothing