{-# LANGUAGE OverloadedStrings, FlexibleContexts, CPP #-}
module Network.Api.Support.Response (
  Responder
, JsonResult (..)
, parseBody
, parseBodyWith
, basicResponder
) where

#if ! MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

#if ! MIN_VERSION_attoparsec(0,14,0)
import qualified Data.ByteString as B
#endif

import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Lazy
import Data.Aeson
import Data.Text

import Network.HTTP.Client
import Network.HTTP.Types

-- | Response handler.
type Responder a =
  Request -> Response BL.ByteString -> a

-- | Wrap up json parse and decode errors.
data JsonResult a =
  ParseError Text | DecodeError Text | JsonSuccess a deriving (Show, Eq)

instance Functor JsonResult where
  fmap _ (ParseError t) = ParseError t
  fmap _ (DecodeError t) = DecodeError t
  fmap f (JsonSuccess a) = JsonSuccess $ f a

instance Applicative JsonResult where
   pure = JsonSuccess

   (JsonSuccess f)   <*> m = fmap f m
   (ParseError err)  <*> _ = ParseError err
   (DecodeError err) <*> _ = DecodeError err

instance Monad JsonResult where
  return = pure
  (ParseError t) >>= _ = ParseError t
  (DecodeError t) >>= _ = DecodeError t
  (JsonSuccess a) >>= f = f a

-- | Parse and decode body handling error cases and success case.
parseBodyWith :: FromJSON a => BL.ByteString -> (Text -> b) -> (Text -> b) -> (a -> b) -> b
parseBodyWith body pHandler dHandler sHandler =
  case parseBody body of
    ParseError t -> pHandler t
    DecodeError t -> dHandler t
    JsonSuccess a -> sHandler a

-- | Parse and decode body.
parseBody :: FromJSON a => BL.ByteString -> JsonResult a
parseBody body =
#if MIN_VERSION_attoparsec(0,14,0)
  case parseOnly json body of
#else
  case parseOnly json (B.concat . BL.toChunks $ body) of
#endif
    Left msg -> ParseError . pack $ msg
    Right j -> case fromJSON j of
      (Error msg') -> DecodeError . pack $ msg'
      (Success a) -> JsonSuccess a

-- | Lift function handling status code and body into a responder.
basicResponder :: (Int -> BL.ByteString -> a) -> Responder a
basicResponder f _ r = f (statusCode (responseStatus r)) (responseBody r)
