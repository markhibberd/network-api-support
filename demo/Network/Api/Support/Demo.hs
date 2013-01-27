{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Network.Api.Support.Demo where

import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import Data.Monoid
import Data.Text

import Network.Api.Support
import Network.HTTP.Conduit
import Network.HTTP.Types

import Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit as C

demo :: IO Text
demo = runRequest def GET "https://api.github.com/gitignore/templates/C" (Endo id) (basicResponder responder)

settings :: ManagerSettings
settings = def { managerCheckCerts = checkDomainOnly }

responder :: Int -> BL.ByteString -> Text
responder _ body = LT.toStrict . LE.decodeUtf8 $ body
