{-# LANGUAGE OverloadedStrings #-}
module Error
where

import Data.Aeson (ToJSON(..), object, (.=))
import Network.HTTP.Types.Status

import qualified Data.Text.Lazy as T
import qualified Web.Scotty.Trans as S

data Error = Error
  { code    :: Status
  , message :: String
  , title   :: String
  }

instance S.ScottyError Error where
  stringError = internalError
  showError (Error _ m _) = T.pack m

instance ToJSON Error where
  toJSON (Error code message title) = object
    [ "error" .= (object
        [ "code"    .= statusCode code
        , "message" .= message
        , "title"   .= title
        ] )
    ]

unauthorized :: String -> Error
unauthorized message = Error status401 message "Not Authorized"

notFound :: String -> Error
notFound message = Error status404 message "Not Found"

internalError :: String -> Error
internalError message = Error status500 message "Internal Server Error"

badRequest :: String -> Error
badRequest message = Error status400 message "Bad Request"
