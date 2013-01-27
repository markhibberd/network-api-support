module Network.Api.Support.Response (
  JsonResult (..)
, parseBody
, parseBodyWith
, basicResponder
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Lazy
import Data.Aeson
import Data.Text

import Network.HTTP.Conduit
import Network.HTTP.Types

data JsonResult a =
  ParseError Text | DecodeError Text | JsonSuccess a deriving (Show, Eq)

instance Functor JsonResult where
  fmap _ (ParseError t) = ParseError t
  fmap _ (DecodeError t) = DecodeError t
  fmap f (JsonSuccess a) = JsonSuccess $ f a

instance Monad JsonResult where
  return = JsonSuccess
  (ParseError t) >>= _ = ParseError t
  (DecodeError t) >>= _ = DecodeError t
  (JsonSuccess a) >>= f = f a

parseBodyWith :: FromJSON a => BL.ByteString -> (Text -> b) -> (Text -> b) -> (a -> b) -> b
parseBodyWith body pHandler dHandler sHandler =
  case parseBody body of
    ParseError t -> pHandler t
    DecodeError t -> dHandler t
    JsonSuccess a -> sHandler a

parseBody :: FromJSON a => BL.ByteString -> JsonResult a
parseBody body =
  case parseOnly json (B.concat . BL.toChunks $ body) of
    Left msg -> ParseError . pack $ msg
    Right j -> case fromJSON j of
      (Error msg') -> DecodeError . pack $ msg'
      (Success a) -> JsonSuccess a

basicResponder :: (Int -> BL.ByteString -> a) -> Response BL.ByteString -> a
basicResponder f (Response (Status code _) _ _ body) =
  f code body
