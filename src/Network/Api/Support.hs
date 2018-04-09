-- |
-- Module:      Network.Api.Support
-- Copyright:   (c) 2012 Mark Hibberd
-- License:     BSD3
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>
-- Portability: portable
--
-- Toolkit for building http client libraries over Network.Http.Conduit
--
module Network.Api.Support
  (
  -- * Request runners
    runRequest
  , runRequest'
  , runRequestWith
  , runRequestWith'

  -- * Request transformers
  , RequestTransformer
  , setApiKey
  , setUrlEncodedBody
  , setQueryParams
  , setHeaders
  , setHeader
  , addHeader
  , stripHeader
  , setCookieJar
  , setMethod
  , setBody
  , setBodyLazy
  , setJson

  -- * Responders
  , Responder
  , JsonResult (..)
  , parseBody
  , parseBodyWith
  , basicResponder

  -- * Compatibility
  , (<>)

  ) where

import Network.Api.Support.Core
import Network.Api.Support.Request
import Network.Api.Support.Response
