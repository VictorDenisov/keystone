{-# LANGUAGE OverloadedStrings #-}
module Error
where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.ByteString.Char8 (unpack)
import Network.HTTP.Types.Status

import qualified Data.Text.Lazy as T
import qualified Web.Scotty.Trans as S

data Error = Error
  { code    :: Status
  , message :: String
  }

instance S.ScottyError Error where
  stringError = internalError
  showError (Error _ m) = T.pack m

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
