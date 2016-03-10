{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Error
where

import Control.Monad.Catch (Exception(..))
import Data.Aeson (ToJSON(..), object, (.=))
import Data.ByteString.Char8 (unpack)
import Data.Data (Typeable)
import Network.HTTP.Types.Status

import qualified Data.Text.Lazy as T
import qualified Web.Scotty.Trans as S

data Error = Error
  { code    :: Status
  , message :: String
  } deriving (Show, Typeable)

data MsgError = EndOfStream
              | ParseFailure
                deriving (Show, Typeable)

instance S.ScottyError Error where
  stringError = internalError
  showError (Error _ m) = T.pack m

instance Exception Error

instance ToJSON Error where
  toJSON (Error code message) = object
    [ "error" .= (object
        [ "code"    .= statusCode code
        , "message" .= message
        , "title"   .= unpack (statusMessage code)
        ] )
    ]

unauthorized :: String -> Error
unauthorized message = Error status401 message

notFound :: String -> Error
notFound message = Error status404 message

internalError :: String -> Error
internalError message = Error status500 message

badRequest :: String -> Error
badRequest message = Error status400 message

conflict :: String -> Error
conflict message = Error status409 message
